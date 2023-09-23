// ignore_for_file: avoid_print

import 'dart:ffi' hide Size;
import 'dart:io';

import 'package:after_layout/after_layout.dart';
import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart' hide Color, ColorSegment;
import 'package:frontend/calculator_bindings.dart' as calculator_bindings
    show ColorSegment;
import 'package:google_fonts/google_fonts.dart';
import 'package:intersperse/intersperse.dart';
import 'package:linked_scroll_controller/linked_scroll_controller.dart';

const String _libName = "dart_bridge";

final DynamicLibrary _dylib = () {
  if (Platform.isMacOS || Platform.isIOS) {
    return DynamicLibrary.open('$_libName.framework/$_libName');
  }
  if (Platform.isAndroid || Platform.isLinux) {
    return DynamicLibrary.open('lib$_libName.so');
  }
  if (Platform.isWindows) {
    return DynamicLibrary.open('$_libName.dll');
  }
  throw UnsupportedError('Unknown platform: ${Platform.operatingSystem}');
}();

final CalculatorBindings bindings = CalculatorBindings(_dylib);

void main() {
  runApp(MaterialApp(
    debugShowCheckedModeBanner: false,
    home: const HomePage(),
    themeMode: ThemeMode.dark,
    theme: ThemeData.dark(useMaterial3: true),
  ));
}

class StyleSegment {
  Color color;
  SourceRange range;
  TextDecoration? decoration;

  StyleSegment(this.color, this.range, {this.decoration});

  factory StyleSegment.fromCalculatorColorSegment(
      calculator_bindings.ColorSegment seg) {
    var arr = seg.color.color;
    var r = arr[0], g = arr[1], b = arr[2], a = arr[3];
    var color = Color(a << 24 | r << 16 | g << 8 | b);
    return StyleSegment(color, seg.range);
  }

  TextStyle textStyle() => TextStyle(
        color: color,
        decoration: decoration,
        decorationColor: color,
      );
}

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> with AfterLayoutMixin {
  final scrollControllers = LinkedScrollControllerGroup();
  late ScrollController inputScrollController;
  late ScrollController resultsScrollController;
  late ScrollController lineNumbersScrollController;

  final resultsController = TextEditingController();
  final inputController = ColoringTextEditingController();
  final lineNumbersController = TextEditingController(text: "1");

  final inputFocusNode = FocusNode();
  final inputUndoController = UndoHistoryController();

  double resultsWidth = 0;

  int calculator = 0;

  @override
  void initState() {
    super.initState();
    inputScrollController = scrollControllers.addAndGet();
    resultsScrollController = scrollControllers.addAndGet();
    lineNumbersScrollController = scrollControllers.addAndGet();
    inputController.addListener(onInputChanged);
  }

  @override
  void afterFirstLayout(BuildContext context) {
    resultsWidth = MediaQuery.of(context).size.width * .25;
    calculator = bindings.create_calculator();
    setState(() {});
  }

  @override
  void dispose() {
    inputScrollController.dispose();
    resultsScrollController.dispose();
    bindings.free_calculator(calculator);
    super.dispose();
  }

  String longestLine(String s) {
    var lines = s.split("\n");
    var linesSorted = lines.indexed.map((e) => (e.$1, e.$2.length)).toList()
      ..sort((a, b) => a.$2.compareTo(b.$2));
    return lines[linesSorted.last.$1];
  }

  Size textDimensions(String text, TextStyle style) {
    var painter = TextPainter(
        text: TextSpan(text: text, style: style),
        maxLines: null,
        textDirection: TextDirection.ltr)
      ..layout(minWidth: 0, maxWidth: double.infinity);
    return painter.size;
  }

  TextStyle get _inputTextStyle =>
      GoogleFonts.sourceCodePro().copyWith(fontSize: 16);

  Widget _wrapInScrollView({
    required Widget widget,
    bool reverse = false,
    double? minWidth,
  }) =>
      SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        reverse: reverse,
        child: IntrinsicWidth(
          child: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              ConstrainedBox(
                constraints: BoxConstraints(
                  minWidth: minWidth ?? MediaQuery.of(context).size.width,
                  maxHeight: 0,
                ),
                child: const SizedBox.expand(),
              ),
              Expanded(child: widget),
            ],
          ),
        ),
      );

  TextField _bareTextField({
    TextEditingController? controller,
    ScrollController? scrollController,
    void Function(String)? onChanged,
    String? hintText,
    bool readOnly = false,
    TextAlign textAlign = TextAlign.start,
    TextInputType? textInputType,
    bool autofocus = false,
    FocusNode? focusNode,
    UndoHistoryController? undoController,
  }) =>
      TextField(
        decoration: InputDecoration(
          border: InputBorder.none,
          hintText: hintText,
          isDense: true,
          constraints: const BoxConstraints(),
          contentPadding: EdgeInsets.zero,
        ),
        controller: controller,
        scrollController: scrollController,
        undoController: undoController,
        autofocus: autofocus,
        focusNode: focusNode,
        readOnly: readOnly,
        keyboardType: textInputType,
        textAlign: textAlign,
        spellCheckConfiguration: const SpellCheckConfiguration.disabled(),
        expands: true,
        style: _inputTextStyle,
        maxLines: null,
        autocorrect: false,
        enableSuggestions: false,
        onChanged: onChanged,
      );

  void onInputChanged() {
    if (calculator == 0) {
      setState(() {});
      return;
    }

    var results = bindings.calculate(
      calculator,
      inputController.text.toNativeUtf8().cast<Char>(),
    );

    var resultsText = "";
    var colorSegments = <StyleSegment>[];

    var lastLine = 0;
    for (int i = 0; i < results.len; i++) {
      var calcRes = results.array.elementAt(i).ref;

      var res = calcRes.data;
      var text = res.str_value.cast<Utf8>().toDartString();
      if (lastLine < res.line_range_start) {
        resultsText += "\n" * (res.line_range_start - lastLine);
      }
      resultsText += text;
      resultsText += "\n" * (res.line_range_end - res.line_range_start);
      lastLine = res.line_range_end;

      for (int j = 0; j < calcRes.color_segments.len; j++) {
        var seg = calcRes.color_segments.array.elementAt(j).ref;
        colorSegments.add(StyleSegment.fromCalculatorColorSegment(seg));
      }

      if (calcRes.data.is_error && calcRes.data.error_ranges.len != 0) {
        for (int k = 0; k < calcRes.data.error_ranges.len; k++) {
          colorSegments.add(StyleSegment(
            Colors.red,
            calcRes.data.error_ranges.array.elementAt(k).ref,
            decoration: TextDecoration.underline,
          ));
        }
      }
    }

    bindings.free_results(results);

    inputController.colorSegments = colorSegments;
    resultsController.text = resultsText;

    lineNumbersController.text = inputController.text
        .split("\n")
        .indexed
        .map((e) => "${e.$1 + 1}")
        .join("\n");
    if (lineNumbersController.text.isEmpty) {
      lineNumbersController.text = "1";
    }

    setState(() {});
  }

  @override
  Widget build(BuildContext context) {
    var lineNumbersLongestLine = longestLine(lineNumbersController.text);
    if (lineNumbersLongestLine.length < 3) lineNumbersLongestLine = "100";

    return Scaffold(
      appBar: AppBar(title: const Text("funcially")),
      body: Column(
        children: [
          Expanded(
            child: Row(
              children: [
                Container(
                  width: textDimensions(lineNumbersLongestLine, _inputTextStyle)
                          .width +
                      4,
                  padding: const EdgeInsets.only(left: 2, right: 2),
                  color: Colors.grey.withOpacity(.05),
                  child: _bareTextField(
                    controller: lineNumbersController,
                    scrollController: lineNumbersScrollController,
                    readOnly: true,
                    textAlign: TextAlign.end,
                  ),
                ),
                const SizedBox(width: 10),
                Expanded(
                  child: _wrapInScrollView(
                    minWidth: textDimensions(
                            longestLine(inputController.text), _inputTextStyle)
                        .width,
                    widget: _bareTextField(
                      controller: inputController,
                      scrollController: inputScrollController,
                      hintText: "Calculate something",
                      textInputType: TextInputType.none,
                      autofocus: true,
                      focusNode: inputFocusNode,
                      undoController: inputUndoController,
                    ),
                  ),
                ),
                Container(
                  decoration: BoxDecoration(
                    border: Border(
                      left: BorderSide(color: Colors.grey.withOpacity(.5)),
                    ),
                    color: Colors.grey.withOpacity(.05),
                  ),
                  padding: const EdgeInsets.only(right: 2, left: 5),
                  width: resultsWidth,
                  child: _wrapInScrollView(
                    reverse: true,
                    minWidth: textDimensions(
                            longestLine(resultsController.text),
                            _inputTextStyle)
                        .width,
                    widget: _bareTextField(
                      controller: resultsController,
                      scrollController: resultsScrollController,
                      readOnly: true,
                      textAlign: TextAlign.end,
                    ),
                  ),
                )
              ],
            ),
          ),
          CalculatorKeyboard(
            targetController: inputController,
            targetUndoController: inputUndoController,
            targetFocusNode: inputFocusNode,
          ),
        ],
      ),
    );
  }
}

class ColoringTextEditingController extends TextEditingController {
  ColoringTextEditingController({this.colorSegments = const []});

  List<StyleSegment> colorSegments;

  List<(String, List<StyleSegment>)> splitIntoLines() {
    var lines = text.split("\n").toList();
    return lines.indexed.map((e) {
      var (i, line) = e;
      return (
        line + (i != lines.length - 1 ? "\n" : ""),
        colorSegments
            .where((seg) => seg.range.start_line <= i && seg.range.end_line > i)
            .map((seg) {
          if (seg.range.start_line != i) {
            seg.range.start_char = 0;
          } else if (seg.range.end_line > i + 1) {
            seg.range.end_char = -1;
          }

          seg.range.start_line = i;
          seg.range.end_line = i + 1;
          return seg;
        }).toList(),
      );
    }).toList();
  }

  List<TextSpan> colorizeLine(
      String line, List<StyleSegment> lineSegments, TextStyle style) {
    if (lineSegments.isEmpty || line.isEmpty) {
      return [
        TextSpan(
          text: line,
          style: style,
        )
      ];
    }

    lineSegments
        .sort((a, b) => a.range.start_char.compareTo(b.range.start_char));

    var result = <TextSpan>[];
    var lastChar = 0;

    for (var seg in lineSegments) {
      var start = seg.range.start_char;
      var end = seg.range.end_char;
      if (end == -1) end = line.length;

      if (lastChar < start) {
        result.add(TextSpan(
          text: line.substring(lastChar, start),
          style: style,
        ));
      }

      result.add(TextSpan(
        text: line.substring(start, end),
        style: style.merge(seg.textStyle()),
      ));

      lastChar = end;
    }

    if (lastChar != line.length) {
      result.add(TextSpan(
        text: line.substring(lastChar),
        style: style,
      ));
    }

    return result;
  }

  @override
  TextSpan buildTextSpan(
      {required BuildContext context,
      TextStyle? style,
      required bool withComposing}) {
    var theStyle = style ?? const TextStyle();
    return TextSpan(
      style: theStyle,
      children: splitIntoLines()
          .expand((el) => colorizeLine(el.$1, el.$2, theStyle))
          .toList(),
    );
  }
}

enum KeyboardType {
  numbers,
  functions,
  normal,
}

extension ToString on KeyboardType {
  String asString() {
    switch (this) {
      case KeyboardType.numbers:
        return "123";
      case KeyboardType.functions:
        return "f(x)";
      case KeyboardType.normal:
        return "ABC";
    }
  }
}

Widget keyboardButton({
  required Widget child,
  void Function()? onPressed,
  bool emphasize = false,
  EdgeInsets padding = const EdgeInsets.all(2),
}) =>
    Padding(
      padding: padding,
      child: ElevatedButton(
        style: ElevatedButton.styleFrom(
          elevation: 0,
          backgroundColor: Colors.grey.withOpacity(emphasize ? .15 : .05),
          shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(5)),
          padding: EdgeInsets.zero,
          shadowColor: Colors.transparent,
        ),
        onPressed: onPressed,
        child: child,
      ),
    );

class TextFieldEditor {
  final TextEditingController controller;
  final UndoHistoryController undoController;
  final FocusNode focusNode;

  const TextFieldEditor({
    required this.controller,
    required this.undoController,
    required this.focusNode,
  });

  void targetRequestFocus() => focusNode.requestFocus();

  void insertText(String text) {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      var textBeforeCursor = controller.text.substring(0, sel.start);
      var textAfterCursor = controller.text.substring(sel.start);
      controller.text = textBeforeCursor + text + textAfterCursor;

      controller.selection = TextSelection.collapsed(
        offset: sel.start + text.length,
        affinity: sel.affinity,
      );
    } else {
      var textBeforeSelection = controller.text.substring(0, sel.start);
      var textAfterSelection = controller.text.substring(sel.end);
      controller.text = textBeforeSelection + text + textAfterSelection;

      controller.selection = TextSelection.collapsed(
        offset: sel.start + text.length,
        affinity: TextAffinity.downstream,
      );
    }
  }

  void insertFunction(String name) {
    insertText("$name()");
    var sel = controller.selection;
    controller.selection = TextSelection.collapsed(
      offset: sel.start - 1,
      affinity: sel.affinity,
    );
  }

  void backspace() {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      if (sel.start == 0) return;
      var textBefore = controller.text.substring(0, sel.start - 1);
      var textAfter = controller.text.substring(sel.start);
      controller.text = textBefore + textAfter;

      controller.selection = TextSelection.collapsed(
        offset: sel.start - 1,
        affinity: sel.affinity,
      );
    } else {
      var textBeforeSelection = controller.text.substring(0, sel.start);
      var textAfterSelection = controller.text.substring(sel.end);
      controller.text = textBeforeSelection + textAfterSelection;

      controller.selection = TextSelection.collapsed(
        offset: sel.start,
        affinity: TextAffinity.downstream,
      );
    }
  }

  void moveSelection(int offset) {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      var newOffset = sel.start + offset;
      if (newOffset < 0 || newOffset > controller.text.length) return;
      sel = TextSelection.collapsed(
        offset: newOffset,
        affinity: sel.affinity,
      );
    } else {
      var newOffset = sel.extentOffset + offset;
      if (newOffset < 0 || newOffset > controller.text.length) return;
      sel = TextSelection(
        baseOffset: sel.baseOffset,
        extentOffset: sel.extentOffset + offset,
      );
    }

    controller.selection = sel;
  }
}

class CalculatorKeyboard extends StatefulWidget {
  const CalculatorKeyboard({
    super.key,
    required this.targetController,
    required this.targetUndoController,
    required this.targetFocusNode,
  });

  final TextEditingController targetController;
  final UndoHistoryController targetUndoController;
  final FocusNode targetFocusNode;

  @override
  State<CalculatorKeyboard> createState() => _CalculatorKeyboardState();
}

class _CalculatorKeyboardState extends State<CalculatorKeyboard> {
  static const Map<KeyboardType, List<List<List<String>>>> layout = {
    KeyboardType.numbers: [
      [
        ["t#f", "t#g", "t#x", "t#y"],
        ["t#pi", "t#e", "f#sqrt", "empty"],
        ["t#^", "t#=", "t#<", "t#>"],
        ["t#in", "empty", "empty", "empty"],
        ["t#.", "t#,", "t#(", "t#)"],
      ],
      [
        ["t#:=", "t#/", "t#*", "t#-"],
        ["t#7", "t#8", "t#9", "t#+"],
        ["t#4", "t#5", "t#6", "backspace"],
        ["t#1", "t#2", "t#3", "return"],
        ["t#0", "space", "left", "right"],
      ],
    ],
    KeyboardType.functions: [
      [
        ["f#sin", "f#cos", "f#tan", "f#cot"],
        ["f#asin", "f#acos", "f#atan", "f#acot"],
        ["f#ln", "f#log", "f#cbrt", "f#root"],
        ["f#abs", "f#floor", "f#ceil", "f#round"],
        ["f#lerp", "f#clamp", "f#map", "empty"],
      ],
      [
        ["t#%", "t#of", "t#!", "t#mod"],
        ["t#&", "t#|", "t#<<", "t#>>"],
        ["t#{", "t#}", "t##", "backspace"],
        ["t#[", "t#]", "t#°", "return"],
        ["t#0b", "t#0x", "left", "right"],
      ],
    ],
  };

  KeyboardType currentKeyboard = KeyboardType.numbers;

  late TextFieldEditor editor;

  @override
  void initState() {
    super.initState();
    editor = TextFieldEditor(
      controller: widget.targetController,
      undoController: widget.targetUndoController,
      focusNode: widget.targetFocusNode,
    );
  }

  Widget keyboardLayoutForType(KeyboardType type) => Expanded(
        child: Row(
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: layout[type]!
              .map((block) {
                return Expanded(
                  child: GridView.builder(
                    gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
                      crossAxisCount: block[0].length,
                    ),
                    physics: const NeverScrollableScrollPhysics(),
                    shrinkWrap: true,
                    itemCount: block[0].length * block.length,
                    itemBuilder: (context, i) {
                      var rowIndex = i ~/ block[0].length;
                      var keyIndex = i - rowIndex * block[0].length;
                      return keyToWidget(block[rowIndex][keyIndex]);
                    },
                  ),
                );
              })
              .cast<Widget>()
              .intersperse(const SizedBox(width: 10))
              .toList(),
        ),
      );

  Widget keyToWidget(String key) {
    var separatorIdx = key.indexOf("#");
    if (separatorIdx == -1) separatorIdx = key.length;
    var type = key.substring(0, separatorIdx);
    switch (type) {
      case "t":
        return keyboardButton(
          child: Text(key.substring(2)),
          onPressed: () => editor.insertText(key.substring(2)),
        );
      case "f":
        return keyboardButton(
          child: Text(key.substring(2)),
          onPressed: () => editor.insertFunction(key.substring(2)),
        );
      case "empty":
        return keyboardButton(child: const SizedBox());
      case "return":
        return keyboardButton(
          child: const Icon(Icons.keyboard_return_outlined),
          onPressed: () => editor.insertText("\n"),
          emphasize: true,
        );
      case "backspace":
        return keyboardButton(
          child: const Icon(Icons.backspace_outlined),
          onPressed: editor.backspace,
          emphasize: true,
        );
      case "space":
        return keyboardButton(
          child: const Icon(Icons.space_bar),
          onPressed: () => editor.insertText(" "),
          emphasize: true,
        );
      case "left":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_left_outlined),
          onPressed: () => editor.moveSelection(-1),
          emphasize: true,
        );
      case "right":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_right_outlined),
          onPressed: () => editor.moveSelection(1),
          emphasize: true,
        );
      default:
        throw "Invalid key type '$type'";
    }
  }

  Widget keyboardUi() {
    switch (currentKeyboard) {
      case KeyboardType.normal:
        return Expanded(
          child: Align(
            alignment: Alignment.bottomCenter,
            child: NormalKeyboardLayout(editor: editor),
          ),
        );
      default:
        return keyboardLayoutForType(currentKeyboard);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: double.infinity,
      height: MediaQuery.of(context).size.height / 2.8,
      color: Colors.grey.withOpacity(.05),
      padding: const EdgeInsets.all(10),
      child: Column(
        children: [
          Row(
            children: [
              SegmentedButton(
                segments: KeyboardType.values
                    .map((key) => ButtonSegment(
                          value: key,
                          label: Text(key.asString()),
                        ))
                    .toList(),
                selected: {currentKeyboard},
                showSelectedIcon: false,
                onSelectionChanged: (newSelection) =>
                    setState(() => currentKeyboard = newSelection.first),
                style: const ButtonStyle(
                  tapTargetSize: MaterialTapTargetSize.shrinkWrap,
                  visualDensity: VisualDensity(horizontal: -3, vertical: -3),
                ),
              ),
              const Spacer(),
              ValueListenableBuilder(
                valueListenable: widget.targetUndoController,
                builder: (context, value, _) {
                  return IconButton(
                    icon: const Icon(Icons.undo),
                    onPressed:
                        value.canUndo ? widget.targetUndoController.undo : null,
                    constraints: const BoxConstraints(),
                    padding: EdgeInsets.zero,
                  );
                },
              ),
              ValueListenableBuilder(
                valueListenable: widget.targetUndoController,
                builder: (context, value, _) => IconButton(
                  icon: const Icon(Icons.redo),
                  onPressed:
                      value.canRedo ? widget.targetUndoController.redo : null,
                  constraints: const BoxConstraints(),
                  padding: EdgeInsets.zero,
                ),
              ),
            ],
          ),
          keyboardUi(),
        ],
      ),
    );
  }
}

class NormalKeyboardLayout extends StatefulWidget {
  const NormalKeyboardLayout({super.key, required this.editor});

  final TextFieldEditor editor;

  @override
  State<NormalKeyboardLayout> createState() => _NormalKeyboardLayoutState();
}

class _NormalKeyboardLayoutState extends State<NormalKeyboardLayout> {
  static const List<List<String>> normalLayout = [
    ["q", "w", "e", "r", "t", "z", "u", "i", "o", "p"],
    ["#3", "a", "s", "d", "f", "g", "h", "j", "k", "l", "#3"],
    ["#shift", "y", "x", "c", "v", "b", "n", "m", "#backspace"],
    ["#switch", ",", ".", "#space", "#left", "#right", "#return"],
  ];

  static const List<List<String>> numberLayout = [
    ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"],
    ["@", "#", "€", "_", "&", "-", "+", "(", ")", "/"],
    ["#switch2", "*", "\"", "'", ":", ";", "!", "?", "#backspace"],
    ["#switch", ",", ".", "#space", "#left", "#right", "#return"],
  ];

  static const List<List<String>> symbolLayout = [
    ["~", "`", "|", "•", "√", "π", "÷", "×", "§", "Δ"],
    ["£", "¥", "\$", "¢", "^", "°", "=", "{", "}", "\\"],
    ["#switch2", "%", "©", "®", "™", "✓", "[", "]", "#backspace"],
    ["#switch", "<", ">", "#space", "#left", "#right", "#return"],
  ];

  bool isCaps = false;
  bool isInNumberLayout = false;
  bool isInSymbolLayout = false;

  Widget _keyboardButton({
    required Widget child,
    void Function()? onPressed,
    bool emphasize = false,
  }) =>
      keyboardButton(
        child: child,
        onPressed: onPressed,
        emphasize: emphasize,
        padding: const EdgeInsets.only(left: 2, right: 2),
      );

  Widget specialKeyTypeToWidget(String type) {
    switch (type) {
      case "shift":
        return _keyboardButton(
          child: const Icon(Icons.arrow_upward_rounded),
          onPressed: () => setState(() => isCaps = !isCaps),
          emphasize: true,
        );
      case "backspace":
        return _keyboardButton(
          child: const Icon(Icons.backspace_outlined),
          onPressed: widget.editor.backspace,
          emphasize: true,
        );
      case "switch":
        return _keyboardButton(
          child: Text(!isInNumberLayout ? "?123" : "ABC"),
          onPressed: () {
            isInNumberLayout = !isInNumberLayout;
            if (!isInNumberLayout) isInSymbolLayout = false;
            setState(() {});
          },
          emphasize: true,
        );
      case "switch2":
        return _keyboardButton(
          child: Text(!isInSymbolLayout ? "=\\<" : "?123"),
          onPressed: () => setState(() => isInSymbolLayout = !isInSymbolLayout),
          emphasize: true,
        );
      case "space":
        return _keyboardButton(
          child: const Icon(Icons.space_bar),
          onPressed: () => widget.editor.insertText(" "),
        );
      case "left":
        return _keyboardButton(
          child: const Icon(Icons.keyboard_arrow_left_outlined),
          onPressed: () => widget.editor.moveSelection(-1),
          emphasize: true,
        );
      case "right":
        return _keyboardButton(
          child: const Icon(Icons.keyboard_arrow_right_outlined),
          onPressed: () => widget.editor.moveSelection(1),
          emphasize: true,
        );
      case "return":
        return _keyboardButton(
          child: const Icon(Icons.keyboard_return_outlined),
          onPressed: () => widget.editor.insertText("\n"),
          emphasize: true,
        );
      default:
        throw "Unknown type '$type'";
    }
  }

  @override
  Widget build(BuildContext context) {
    var layout = !isInNumberLayout
        ? normalLayout
        : !isInSymbolLayout
            ? numberLayout
            : symbolLayout;

    return Column(
      mainAxisSize: MainAxisSize.min,
      children: layout
          .map((row) => Row(
                children: row.map((String key) {
                  if (!key.startsWith("#")) {
                    var symbol =
                        !isCaps ? key.toLowerCase() : key.toUpperCase();
                    return Expanded(
                      child: _keyboardButton(
                        child: Text(symbol),
                        onPressed: () => widget.editor.insertText(symbol),
                      ),
                    );
                  } else {
                    var keyType = key.substring(1);
                    var spacingMultiplier = int.tryParse(keyType);
                    if (spacingMultiplier != null) {
                      return SizedBox(width: 5.0 * spacingMultiplier);
                    } else {
                      return Expanded(child: specialKeyTypeToWidget(keyType));
                    }
                  }
                }).toList(),
              ))
          .toList(),
    );
  }
}
