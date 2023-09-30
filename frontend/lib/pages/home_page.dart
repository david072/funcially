import 'dart:ffi' hide Size;

import 'package:after_layout/after_layout.dart';
import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:frontend/main.dart';
import 'package:frontend/pages/plot_page.dart';
import 'package:frontend/pages/settings_page.dart';
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

  bool initFinished = false;
  bool loading = true;

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
    // if (calculator != 0) {
    //   settings.saveSettingsToCalculator(calculator);
    //   setState(() => loading = false);
    // }

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
    // calculator = bindings.create_calculator();
    // if (initFinished) {
    //   settings.saveSettingsToCalculator(calculator);
    //   setState(() => loading = false);
    // }
  }

  @override
  void dispose() {
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

    bindings.reset_calculator(calculator);
    var results = bindings.calculate(
      calculator,
      inputController.text.toNativeUtf8().cast<Char>(),
      settings.useThousandsSeparator,
    );

    var resultsText = "";
    var resultsLineNumbersText = "";
    var colorSegments = <StyleSegment>[];

    var lastLine = 0;
    for (int i = 0; i < results.len; i++) {
      var calcRes = results.array.elementAt(i).ref;

      var res = calcRes.data;
      var text = res.str_value.cast<Utf8>().toDartString();
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
            range: calcRes.data.error_ranges.array.elementAt(k).ref,
            decoration: TextDecoration.underline,
          ));
        }
      } else {
        for (int j = 0; j < calcRes.color_segments.len; j++) {
          var seg = calcRes.color_segments.array.elementAt(j).ref;
          colorSegments.add(StyleSegment.fromCalculatorColorSegment(seg));
        }
      }
    }

    bindings.free_results(results);

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
              MaterialPageRoute(builder: (_) => const PlotPage()),
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
                children: [
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
                        Container(
                          decoration: BoxDecoration(
                            border: Border(
                              left: BorderSide(
                                  color: Colors.grey.withOpacity(.5)),
                            ),
                            color: Colors.grey.withOpacity(.05),
                          ),
                          padding: const EdgeInsets.only(right: 2),
                          width: MediaQuery.of(context).size.width * .35,
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
                        )
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
