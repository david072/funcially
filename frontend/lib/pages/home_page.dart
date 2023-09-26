import 'dart:ffi' hide Size;

import 'package:after_layout/after_layout.dart';
import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:frontend/main.dart';
import 'package:frontend/util/coloring_text_editing_controller.dart';
import 'package:frontend/util/util.dart';
import 'package:frontend/widgets/custom_keyboard.dart';
import 'package:google_fonts/google_fonts.dart';
import 'package:linked_scroll_controller/linked_scroll_controller.dart';

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

  late TextFieldEditor editor;

  int calculator = 0;

  @override
  void initState() {
    super.initState();
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
  }

  bool onHardwareEvent(KeyEvent event) {
    if (event.character != null) {
      editor.insertText(event.character!);
    }
    return true;
  }

  @override
  void afterFirstLayout(BuildContext context) {
    calculator = bindings.create_calculator();
    setState(() {});
  }

  @override
  void dispose() {
    inputScrollController.dispose();
    resultsScrollController.dispose();
    bindings.free_calculator(calculator);
    HardwareKeyboard.instance.removeHandler(onHardwareEvent);
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

    bindings.reset_calculator(calculator);
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
      body: SafeArea(
        child: Column(
          children: [
            Expanded(
              child: Row(
                children: [
                  Container(
                    width:
                        textDimensions(lineNumbersLongestLine, _inputTextStyle)
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
                              longestLine(inputController.text),
                              _inputTextStyle)
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
                    width: MediaQuery.of(context).size.width * .25,
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
      ),
    );
  }
}
