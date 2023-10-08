import 'dart:ffi' hide Size;
import 'dart:math';

import 'package:after_layout/after_layout.dart';
import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:frontend/calculator_bindings.dart' as calculator_bindings;
import 'package:frontend/main.dart';
import 'package:frontend/pages/settings_page.dart';
import 'package:frontend/util/coloring_text_editing_controller.dart';
import 'package:frontend/util/menu_entry.dart';
import 'package:frontend/util/util.dart';
import 'package:frontend/widgets/custom_keyboard.dart';
import 'package:frontend/widgets/plot.dart';
import 'package:frontend/widgets/resizable_container.dart';
import 'package:google_fonts/google_fonts.dart';
import 'package:linked_scroll_controller/linked_scroll_controller.dart';

const minTabletWidth = 700;

class Result {
  final String text;
  final (int, int) lineRange;

  const Result(this.text, this.lineRange);

  Result.fromCalculatorResult(calculator_bindings.FfiCalculatorResult result)
      : text = result.data.str_value.cast<Utf8>().toDartString().trim(),
        lineRange = (result.data.line_range_start, result.data.line_range_end);
}

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> with AfterLayoutMixin {
  ShortcutRegistryEntry? shortcutEntry;

  final calculatorKeyboardKey = GlobalKey<CalculatorKeyboardState>();

  final scrollControllers = LinkedScrollControllerGroup();
  late ScrollController inputScrollController;
  late ScrollController resultsScrollController;
  late ScrollController lineNumbersScrollController;

  final resultsController = TextEditingController();
  final inputController = ColoringTextEditingController();
  final lineNumbersController = TextEditingController(text: "1");
  final resultsLineNumbersController = TextEditingController();

  final inputFocusNode = FocusNode();
  final inputUndoController = UndoHistoryController();

  late TextFieldEditor editor;

  int calculator = 0;
  late CalculatorSettings settings;

  List<Result> results = [];
  List<PlotGraph> graphs = [];

  bool initFinished = false;
  bool loading = true;

  double? resultsWidth;
  double? plotWidth;

  @override
  void initState() {
    super.initState();
    initialize();
  }

  Future<void> initialize() async {
    inputScrollController = scrollControllers.addAndGet();
    resultsScrollController = scrollControllers.addAndGet();
    lineNumbersScrollController = scrollControllers.addAndGet();
    inputController.addListener(onInputChanged);

    editor = TextFieldEditor(
      focusNode: inputFocusNode,
      controller: inputController,
      undoController: inputUndoController,
    );

    HardwareKeyboard.instance.addHandler(onHardwareEvent);

    settings = await CalculatorSettings.load();
    if (calculator != 0) {
      settings.saveSettingsToCalculator(calculator);
      setState(() => loading = false);
    }

    initFinished = true;
  }

  bool onHardwareEvent(KeyEvent event) {
    calculatorKeyboardKey.currentState?.setShowKeyboard(false);
    if (event.character != null) {
      editor.insertText(event.character!);
    }
    return true;
  }

  @override
  void afterFirstLayout(BuildContext context) {
    calculator = bindings.create_calculator();
    if (initFinished) {
      settings.saveSettingsToCalculator(calculator);
      setState(() => loading = false);
    }
  }

  @override
  void dispose() {
    shortcutEntry?.dispose();
    inputScrollController.dispose();
    resultsScrollController.dispose();
    bindings.free_calculator(calculator);
    HardwareKeyboard.instance.removeHandler(onHardwareEvent);
    super.dispose();
  }

  TextStyle get _inputTextStyle =>
      GoogleFonts.sourceCodePro().copyWith(fontSize: 16);

  void onInputChanged() {
    if (calculator == 0) {
      setState(() {});
      return;
    }

    this.results.clear();

    bindings.reset_calculator(calculator);
    var results = bindings.calculate(
      calculator,
      inputController.text.toNativeUtf8().cast<Char>(),
      settings.useThousandsSeparator,
    );

    var resultsText = "";
    var resultsLineNumbersText = "";
    var colorSegments = <StyleSegment>[];
    var newGraphs = <PlotGraph>[];

    var lastLine = 0;
    for (int i = 0; i < results.len; i++) {
      var calcRes = results.array.elementAt(i).ref;

      var res = calcRes.data;
      var text = res.str_value.cast<Utf8>().toDartString().trim();
      if (lastLine < res.line_range_start) {
        var topOffset = "\n" * (res.line_range_start - lastLine);
        resultsText += topOffset;
        resultsLineNumbersText += topOffset;
      }

      resultsText += text;
      if (text.isNotEmpty) {
        resultsLineNumbersText += "${res.line_range_start + 1}";
      }

      var bottomOffset = "\n" * (res.line_range_end - res.line_range_start);
      resultsText += bottomOffset;
      resultsLineNumbersText += bottomOffset;
      lastLine = res.line_range_end;

      if (calcRes.data.is_error && calcRes.data.error_ranges.len != 0) {
        for (int k = 0; k < calcRes.data.error_ranges.len; k++) {
          colorSegments.add(StyleSegment(
            color: Colors.red,
            range: SourceRange.fromCalculatorRange(
              calcRes.data.error_ranges.array.elementAt(k).ref,
            ),
            decoration: TextDecoration.underline,
          ));
        }
      } else {
        for (int j = 0; j < calcRes.color_segments.len; j++) {
          var seg = calcRes.color_segments.array.elementAt(j).ref;
          colorSegments.add(StyleSegment.fromCalculatorColorSegment(seg));
        }
      }

      if (res.function_name.address != 0 && res.function_argument_count == 1) {
        var name = res.function_name.cast<Utf8>().toDartString();
        var index = graphs.indexWhere((g) => g.name == name);
        if (index == -1) {
          newGraphs.add(PlotGraph(
            name: name,
            color: Colors.primaries[Random().nextInt(Colors.primaries.length)],
            function: (x) {
              return bindings.calculate_function_1(
                calculator,
                name.toNativeUtf8().cast<Char>(),
                x,
              );
            },
          ));
        } else {
          newGraphs.add(graphs[index]);
        }
      }

      this.results.add(Result.fromCalculatorResult(calcRes));
    }

    bindings.free_results(results);

    graphs = newGraphs;
    inputController.colorSegments = colorSegments;
    resultsController.text = resultsText;
    resultsLineNumbersController.text = resultsLineNumbersText;

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

  List<MenuEntry> getMenus() {
    final result = [
      MenuEntry(
        label: "Edit",
        children: [
          MenuEntry(
            label: "Surround selection with brackets",
            onPressed: surroundSelectionWithBrackets,
            shortcut:
                const SingleActivator(LogicalKeyboardKey.keyB, control: true),
          ),
          MenuEntry(
            label: "Copy Result",
            onPressed: copyResult,
            shortcut: const SingleActivator(LogicalKeyboardKey.keyC,
                control: true, shift: true),
          ),
          MenuEntry(
            label: "Format",
            onPressed: formatInput,
            shortcut: const SingleActivator(LogicalKeyboardKey.keyF,
                control: true, shift: true),
          ),
        ],
      ),
    ];

    shortcutEntry?.dispose();
    shortcutEntry =
        ShortcutRegistry.of(context).addAll(MenuEntry.shortcuts(result));

    return result;
  }

  void surroundSelectionWithBrackets() {
    var sel = inputController.selection;
    if (sel.isCollapsed) return;

    var text = inputController.text;
    inputController.text =
        "${sel.textBefore(text)}(${sel.textInside(text)})${sel.textAfter(text)}";
  }

  Future<void> copyResult() async {
    var line = inputController.selection.start;
    if (inputController.selection.affinity == TextAffinity.upstream) line--;

    var text = results
        .firstWhereOrNull(
            (res) => res.lineRange.$1 <= line && res.lineRange.$2 > line)
        .map((it) => it.text);
    if (text == null) return;

    await Clipboard.setData(ClipboardData(text: text));
    if (!mounted) return;
    ScaffoldMessenger.of(context)
        .showSnackBar(const SnackBar(content: Text("Copied result")));
  }

  void formatInput() {
    var result = bindings.format(
        calculator, inputController.text.toNativeUtf8().cast<Char>());
    // format failed
    if (result.address == 0) return;
    inputController.text = result.cast<Utf8>().toDartString();
    bindings.free_str(result);
  }

  @override
  Widget build(BuildContext context) {
    var lineNumbersLongestLine = longestLine(lineNumbersController.text);
    if (lineNumbersLongestLine.length < 3) lineNumbersLongestLine = "100";

    return Scaffold(
      appBar: AppBar(
        title: const Text("funcially"),
        actions: [
          IconButton(
            onPressed: () => Navigator.push(
              context,
              MaterialPageRoute(
                builder: (_) => PlotPage(graphs: graphs.toList()),
              ),
            ),
            icon: const Icon(Icons.show_chart),
          ),
          IconButton(
            onPressed: () async {
              await Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (_) => SettingsPage(
                    calculator: calculator,
                    monospaceTextStyle: _inputTextStyle,
                  ),
                ),
              );
              settings = await CalculatorSettings.load();
              onInputChanged();
            },
            icon: const Icon(Icons.settings_outlined),
          ),
        ],
      ),
      body: !loading
          ? SafeArea(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  MenuBar(
                    style: const MenuStyle(
                      elevation: MaterialStatePropertyAll(0),
                    ),
                    children: MenuEntry.build(getMenus()),
                  ),
                  Expanded(
                    child: Row(
                      children: [
                        _LineNumberColumn(
                          lineNumbersController: lineNumbersController,
                          style: _inputTextStyle,
                          scrollController: lineNumbersScrollController,
                        ),
                        const SizedBox(width: 10),
                        Expanded(
                          child: wrapInScrollView(
                            context: context,
                            minWidth: textDimensions(
                                    longestLine(inputController.text),
                                    _inputTextStyle)
                                .width,
                            widget: bareTextField(
                              controller: inputController,
                              scrollController: inputScrollController,
                              hintText: "Calculate something",
                              textInputType: TextInputType.none,
                              autofocus: true,
                              focusNode: inputFocusNode,
                              undoController: inputUndoController,
                              textStyle: _inputTextStyle,
                            ),
                          ),
                        ),
                        ResizableContainer(
                          color: Colors.grey.withOpacity(.05),
                          padding: const EdgeInsets.only(right: 2),
                          initialWidth: MediaQuery.of(context).size.width * .35,
                          child: Row(
                            mainAxisSize: MainAxisSize.min,
                            children: [
                              _LineNumberColumn(
                                lineNumbersController:
                                    resultsLineNumbersController,
                                style: _inputTextStyle,
                                scrollController: resultsScrollController,
                              ),
                              const SizedBox(width: 5),
                              Expanded(
                                child: wrapInScrollView(
                                  context: context,
                                  reverse: true,
                                  minWidth: textDimensions(
                                          longestLine(resultsController.text),
                                          _inputTextStyle)
                                      .width,
                                  widget: bareTextField(
                                    controller: resultsController,
                                    scrollController: resultsScrollController,
                                    readOnly: true,
                                    textAlign: TextAlign.end,
                                    textStyle: _inputTextStyle,
                                  ),
                                ),
                              ),
                            ],
                          ),
                        ),
                        if (MediaQuery.sizeOf(context).width >= minTabletWidth)
                          ResizableContainer(
                            initialWidth: MediaQuery.sizeOf(context).width * .2,
                            child: PlotWidget(
                              graphs: graphs,
                            ),
                          ),
                      ],
                    ),
                  ),
                  CalculatorKeyboard(
                    key: calculatorKeyboardKey,
                    editor: editor,
                  ),
                ],
              ),
            )
          : const Center(child: CircularProgressIndicator()),
    );
  }
}

class _LineNumberColumn extends StatelessWidget {
  const _LineNumberColumn({
    super.key,
    required this.lineNumbersController,
    required this.style,
    this.scrollController,
  });

  final TextEditingController lineNumbersController;
  final TextStyle style;
  final ScrollController? scrollController;

  @override
  Widget build(BuildContext context) {
    var lineNumbersLongestLine = longestLine(lineNumbersController.text);
    if (lineNumbersLongestLine.length < 3) lineNumbersLongestLine = "100";

    return Container(
      width: textDimensions(lineNumbersLongestLine, style).width + 4,
      padding: const EdgeInsets.only(left: 2, right: 2),
      color: Colors.grey.withOpacity(.05),
      child: bareTextField(
        controller: lineNumbersController,
        scrollController: scrollController,
        readOnly: true,
        textAlign: TextAlign.end,
        textStyle: style,
      ),
    );
  }
}

class PlotPage extends StatelessWidget {
  const PlotPage({super.key, required this.graphs});

  final List<PlotGraph> graphs;

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Plot")),
      body: PlotWidget(
        graphs: graphs,
      ),
    );
  }
}
