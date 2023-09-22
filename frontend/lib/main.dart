// ignore_for_file: avoid_print

import 'dart:ffi' hide Size;
import 'dart:io';

import 'package:after_layout/after_layout.dart';
import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart';
import 'package:google_fonts/google_fonts.dart';
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

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> with AfterLayoutMixin {
  final scrollControllers = LinkedScrollControllerGroup();
  late ScrollController inputScrollController;
  late ScrollController resultsScrollController;

  final resultsController = TextEditingController();

  String input = "";

  double resultsWidth = 0;

  int calculator = 0;

  @override
  void initState() {
    super.initState();
    inputScrollController = scrollControllers.addAndGet();
    resultsScrollController = scrollControllers.addAndGet();
  }

  @override
  void afterFirstLayout(BuildContext context) {
    resultsWidth = MediaQuery.of(context).size.width * .25;
    print("Creating calculator");
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
    var lines = input.split("\n");
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
                  maxHeight: 0,
                  minWidth: minWidth ?? MediaQuery.of(context).size.width,
                ),
                child: Text(longestLine(input), style: _inputTextStyle),
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
        readOnly: readOnly,
        textAlign: textAlign,
        spellCheckConfiguration: const SpellCheckConfiguration.disabled(),
        expands: true,
        style: _inputTextStyle,
        maxLines: null,
        autocorrect: false,
        enableSuggestions: false,
        onChanged: onChanged,
      );

  void onInputChanged(String newInput) {
    input = newInput;

    if (calculator == 0) {
      setState(() {});
      return;
    }

    print("onInputChanged");
    var results = bindings.calculate(
      calculator,
      input.toNativeUtf8().cast<Char>(),
    );

    var resultsText = "";
    var lastLine = 0;
    for (int i = 0; i < results.len; i++) {
      var res = results.array.elementAt(i).ref;
      var text = res.str_value.cast<Utf8>().toDartString();
      if (lastLine < res.line_range_start) {
        resultsText += "\n" * (res.line_range_start - lastLine);
      }
      resultsText += text;
      resultsText += "\n" * (res.line_range_end - res.line_range_start);
      lastLine = res.line_range_end;
    }

    bindings.free_results(results);

    resultsController.text = resultsText;
    setState(() {});
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("funcially")),
      body: Padding(
        padding: const EdgeInsets.all(10),
        child: Stack(
          children: [
            _wrapInScrollView(
              widget: _bareTextField(
                scrollController: inputScrollController,
                hintText: "Calculate something",
                onChanged: onInputChanged,
              ),
            ),
            Positioned(
              top: 0,
              right: 0,
              bottom: 0,
              width: resultsWidth,
              child: Container(
                decoration: BoxDecoration(
                  border: Border(
                    left: BorderSide(color: Colors.grey.withOpacity(.5)),
                  ),
                  color: Colors.grey.withAlpha(15),
                ),
                padding: const EdgeInsets.only(right: 2, left: 5),
                width: resultsWidth,
                child: _wrapInScrollView(
                  reverse: true,
                  minWidth: textDimensions(
                          longestLine(resultsController.text), _inputTextStyle)
                      .width,
                  widget: _bareTextField(
                    controller: resultsController,
                    scrollController: resultsScrollController,
                    readOnly: true,
                    textAlign: TextAlign.end,
                  ),
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
}
