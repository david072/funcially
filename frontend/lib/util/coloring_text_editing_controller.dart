import 'dart:convert';
import 'dart:ffi';
import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart' hide Color;

class StyleSegment {
  final Color color;
  final SourceRange? range;
  final TextDecoration? decoration;
  final FontStyle? fontStyle;

  const StyleSegment({
    required this.color,
    this.range,
    this.decoration,
    this.fontStyle,
  });

  static const commentStyle = StyleSegment(
    color: Colors.grey,
    fontStyle: FontStyle.italic,
  );

  factory StyleSegment.fromCalculatorColorSegment(ColorSegment seg) {
    var arr = seg.color.color;
    var r = arr[0], g = arr[1], b = arr[2], a = arr[3];
    var color = Color(a << 24 | r << 16 | g << 8 | b);
    return StyleSegment(color: color, range: seg.range);
  }

  TextStyle textStyle() => TextStyle(
        color: color,
        decoration: decoration,
        decorationColor: color,
        fontStyle: fontStyle,
      );
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
            .where((seg) => seg.range != null)
            .where(
                (seg) => seg.range!.start_line <= i && seg.range!.end_line > i)
            .map((seg) {
          if (seg.range!.start_line != i) {
            seg.range!.start_char = 0;
          } else if (seg.range!.end_line > i + 1) {
            seg.range!.end_char = -1;
          }

          seg.range!.start_line = i;
          seg.range!.end_line = i + 1;
          return seg;
        }).toList(),
      );
    }).toList();
  }

  List<TextSpan> colorizeLine(
      String line, List<StyleSegment> lineSegments, TextStyle style) {
    if (line.trim().startsWith("#")) {
      return [
        TextSpan(
          text: line,
          style: StyleSegment.commentStyle.textStyle(),
        ),
      ];
    }

    if (lineSegments.isEmpty || line.isEmpty) {
      return [
        TextSpan(
          text: line,
          style: style,
        )
      ];
    }

    lineSegments
        .sort((a, b) => a.range!.start_char.compareTo(b.range!.start_char));

    // print(lineSegments
    //     .map((s) => "${s.range.start_char}..${s.range.end_char}")
    //     .join(",\n"));

    var result = <TextSpan>[];
    var lastChar = 0;

    for (var seg in lineSegments) {
      var start = utf8IndexToCharIndex(line, seg.range!.start_char);
      var end = utf8IndexToCharIndex(line, seg.range!.end_char);
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

    if (line.substring(lastChar).contains("#")) {
      result.add(TextSpan(
        text: line.substring(lastChar),
        style: StyleSegment.commentStyle.textStyle(),
      ));
    } else if (lastChar != line.length) {
      result.add(TextSpan(
        text: line.substring(lastChar),
        style: style,
      ));
    }

    return result;
  }

  int utf8IndexToCharIndex(String str, int utf8Index) {
    var utf8Str = utf8.encode(str);
    var utf8Offset = 0;
    var codeUnits = str.codeUnits;
    for (int i = 0; i < utf8Index; i++) {
      if (i == codeUnits.length) break;
      if (utf8Str[i + utf8Offset] == codeUnits[i]) continue;

      // If the utf8 codepoint does not align with the code unit,
      // subtrace 1 from the rust index, since the character has
      // been split into two indices in utf8.
      utf8Index -= 1;
      utf8Offset += 1;
    }

    return utf8Index;
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
