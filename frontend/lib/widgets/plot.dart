import 'dart:math';

import 'package:flutter/material.dart';

double log10(num x) => log(x) / ln10;

double map(double n, double a1, double b1, double a2, double b2) =>
    (n - a1) * (b2 - a2) / (b1 - a1) + a2;

/// Converts the double to a string and removes trailing zeroes.
///
/// e.g. "1.00" => "1"
String doubleToString(double d) =>
    d.toString().replaceAll(RegExp(r"\.0*(?!\d)"), "");

enum _PositionMode {
  /// The given Offset is the bottom left corner
  bottomLeft,

  /// The given Offset is the bottom right corner
  bottomRight,

  /// The given Offset is the bottom left corner by default,
  /// however if there isn't enough space, use the bottom right corner instead.
  bottomLeftJustifyRight,

  /// The given Offset is the bottom left corner by default,
  /// however if there isn't enough space, use the top left corner instead.
  bottomLeftJustifyTop,
}

class _AxisBounds {
  double min;
  double max;

  _AxisBounds({required this.min, required this.max});

  double get length => max - min;

  (double, double) record() => (min, max);

  bool contains(double n) => n <= max && n >= min;

  void scale(double factor) {
    // TODO: Check if this works when panning everywhere, and not just in the middle of the screen
    var rangeMiddle = min + (max - min) / 2;
    min = (min - rangeMiddle) / factor + rangeMiddle;
    max = (max - rangeMiddle) / factor + rangeMiddle;
  }
}

class _PlotBounds {
  _AxisBounds x;
  _AxisBounds y;

  _PlotBounds({required this.x, required this.y});
}

class _PlotPainter extends CustomPainter {
  static const baseColor = Colors.white;
  static const minLargeIntervalDistance = 14.0;
  static const maxLargeIntervalDistance = 50.0;

  final _PlotBounds bounds;

  _PlotPainter({
    required this.bounds,
  });

  Paint get baseIntervalPaint => Paint()
    ..color = baseColor
    ..strokeWidth = 1;

  @override
  void paint(Canvas canvas, Size size) {
    drawGrid(canvas, size);
    drawLabels(canvas, size);

    // canvas.drawLine(Offset(0, 10), Offset(size.width, 10), baseIntervalPaint);
    // drawText(
    //   "10",
    //   Offset(size.width, 10),
    //   canvas,
    //   size,
    //   positionMode: _PositionMode.bottomRight,
    // );
  }

  void drawGrid(Canvas canvas, Size size) {
    intervalDraw(
      canvas,
      size,
      bounds.y,
      yNumberToPixel,
      (_, y, paint) =>
          canvas.drawLine(Offset(0, y), Offset(size.width, y), paint),
    );

    intervalDraw(
      canvas,
      size,
      bounds.x,
      xNumberToPixel,
      (_, x, paint) =>
          canvas.drawLine(Offset(x, 0), Offset(x, size.height), paint),
    );
  }

  void drawLabels(Canvas canvas, Size size) {
    double yAxisPosition;
    if (bounds.x.contains(0)) {
      yAxisPosition = xNumberToPixel(0, size);
    } else if (bounds.x.min > 0 && bounds.x.max > 0) {
      yAxisPosition = 0;
    } else {
      yAxisPosition = size.width;
    }

    intervalDraw(
      canvas,
      size,
      bounds.y,
      yNumberToPixel,
      (n, y, paint) {
        // skip n = 0, since we use the x axis to draw it
        if (n == 0) return;
        drawText(
          doubleToString(n),
          Offset(yAxisPosition, y),
          canvas,
          size,
          textStyle: TextStyle(
            fontSize: 12,
            color: paint.color,
          ),
          positionMode: _PositionMode.bottomLeftJustifyRight,
        );
      },
    );

    double xAxisPosition;
    if (bounds.y.contains(0)) {
      xAxisPosition = yNumberToPixel(0, size);
    } else if (bounds.y.min > 0 && bounds.y.max > 0) {
      xAxisPosition = 0;
    } else {
      xAxisPosition = size.height;
    }

    intervalDraw(
      canvas,
      size,
      bounds.x,
      xNumberToPixel,
      (n, x, paint) => drawText(
        doubleToString(n),
        Offset(x + 2.5, xAxisPosition),
        canvas,
        size,
        textStyle: TextStyle(
          fontSize: 12,
          color: paint.color,
        ),
        positionMode: _PositionMode.bottomLeftJustifyTop,
      ),
    );
  }

  void intervalDraw(
    Canvas canvas,
    Size size,
    _AxisBounds bounds,
    double Function(double, Size) numberToPixelFn,
    void Function(double, double, Paint) draw,
  ) {
    var (min, max) = bounds.record();
    var (baseInterval, smallInterval) = calculateIntervals(min, max);

    var smallIntervalPaint =
        getSmallIntervalPaint(smallInterval, size, numberToPixelFn);

    var startOffset = calculateStartOffset(smallInterval, min);
    for (double n = startOffset + min; n <= max; n += smallInterval) {
      var px = numberToPixelFn(n, size);
      var isBaseInterval = n % baseInterval == 0;
      draw(n, px, isBaseInterval ? baseIntervalPaint : smallIntervalPaint);
    }
  }

  (double, double) calculateIntervals(double min, double max) {
    // log10(interval length) gives us the magnitude of the numbers in the range
    // i.e. [-12; 12] would have a length of 24, and floor(log10(24)) = 1
    // our step size is then 10 to the power of that, meaning 10^1.
    var power = log10(max - min).floorToDouble();
    var smallInterval = pow(10.0, power - 1) as double;
    var baseInterval = pow(10.0, power) as double;
    return (baseInterval, smallInterval);
  }

  double calculateStartOffset(double interval, double min) {
    // mod gives us the offset from the previous multiple of interval to min
    // e.g. -12 % 10 = 8, since it'd be -10 --> -12, or 6 % 10 = 6, since 0 --> 6
    //                                       8                              6
    // If we subtract that from the interval, we know how much further we need
    // to go to reach the first multiple of interval from the "left". Then, we
    // can iterate in interval steps to draw the lines.
    return interval - (min % interval);
  }

  /// Calculates the color for the small interval. It does this, by calculating the
  /// pixel distance between two ticks of the large interval and clamping it within
  /// [minLargeIntervalDistance;maxLargeIntervalDistance]. Then, it maps the resulting
  /// number from the clamping range onto a [0;1] range, which is used as the opacity.
  Paint getSmallIntervalPaint(
    double smallInterval,
    Size size,
    double Function(double, Size) numberToPixel,
  ) {
    var p1 = numberToPixel(smallInterval, size);
    var p2 = numberToPixel(smallInterval * 2, size);
    var distanceBetweenLargeIntervalTicks =
        (p2 - p1).clamp(minLargeIntervalDistance, maxLargeIntervalDistance);

    var smallIntervalPaintOpacity = map(
      distanceBetweenLargeIntervalTicks,
      minLargeIntervalDistance,
      maxLargeIntervalDistance,
      0,
      1,
    );
    return Paint()
      ..color = baseColor.withOpacity(smallIntervalPaintOpacity)
      ..strokeWidth = 1;
  }

  double xNumberToPixel(double x, Size size) =>
      map(x, bounds.x.min, bounds.x.max, 0, size.width);

  double yNumberToPixel(double y, Size size) =>
      map(y, bounds.y.min, bounds.y.max, 0, size.height);

  void drawText(
    String text,
    Offset pos,
    Canvas canvas,
    Size size, {
    TextStyle? textStyle,
    _PositionMode positionMode = _PositionMode.bottomLeft,
  }) {
    var painter = TextPainter(
      text: TextSpan(text: text, style: textStyle),
      textDirection: TextDirection.ltr,
    );
    painter.layout(minWidth: 0, maxWidth: size.width);

    Offset bottomLeftOffset() => Offset(pos.dx, pos.dy - painter.size.height);
    Offset bottomRightOffset() =>
        Offset(pos.dx - painter.size.width, pos.dy - painter.size.height);

    switch (positionMode) {
      case _PositionMode.bottomLeft:
        pos = bottomLeftOffset();
        break;
      case _PositionMode.bottomRight:
        pos = bottomRightOffset();
        break;
      case _PositionMode.bottomLeftJustifyRight:
        if (bottomLeftOffset().dx + painter.size.width >= size.width) {
          pos = bottomRightOffset();
        } else {
          pos = bottomLeftOffset();
        }
        break;
      case _PositionMode.bottomLeftJustifyTop:
        pos = bottomLeftOffset();
        if (pos.dy - painter.size.height < 0) {
          pos = Offset(pos.dx, pos.dy + painter.size.height);
        }
        break;
    }

    painter.paint(canvas, pos);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => true;
}

class PlotWidget extends StatefulWidget {
  const PlotWidget({super.key});

  @override
  State<PlotWidget> createState() => _PlotWidgetState();
}

class _PlotWidgetState extends State<PlotWidget> {
  _PlotBounds? bounds;

  late Offset previousScalePos;
  late double lastScalingFactor;

  void panBounds(ScaleUpdateDetails details, BoxConstraints constraints) {
    var delta = details.localFocalPoint - previousScalePos;

    // x panning
    var diff = delta.dx * (bounds!.x.length / constraints.maxWidth);
    bounds!.x.min -= diff;
    bounds!.x.max -= diff;

    // y panning
    diff = delta.dy * (bounds!.y.length / constraints.maxHeight);
    bounds!.y.min -= diff;
    bounds!.y.max -= diff;

    previousScalePos = details.localFocalPoint;
  }

  void scaleBounds(ScaleUpdateDetails details, BoxConstraints constraints) {
    var scalingFactor = details.scale / lastScalingFactor;
    bounds!.x.scale(scalingFactor);
    bounds!.y.scale(scalingFactor);
    lastScalingFactor = details.scale;
  }

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraints) {
      if (bounds == null) {
        var aspectRatio = constraints.maxHeight / constraints.maxWidth;
        var x = _AxisBounds(min: -12, max: 12);
        var y = _AxisBounds(min: x.min * aspectRatio, max: x.max * aspectRatio);
        bounds = _PlotBounds(x: x, y: y);
      }

      return GestureDetector(
        // onTap: () {
        //   bounds!.x.scale(.5);
        //   bounds!.y.scale(.5);
        // },
        onScaleStart: (details) {
          previousScalePos = details.localFocalPoint;
          lastScalingFactor = 1;
        },
        onScaleUpdate: (details) {
          panBounds(details, constraints);
          scaleBounds(details, constraints);
          setState(() {});
        },
        child: CustomPaint(
          painter: _PlotPainter(
            bounds: bounds!,
          ),
          size: Size.infinite,
        ),
      );
    });
  }
}
