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

  final resultsController = TextEditingController();
  final inputController = ColoringTextEditingController();

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

    var results = bindings.calculate(
      calculator,
      input.toNativeUtf8().cast<Char>(),
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
                controller: inputController,
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
