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
  bottomLeft,
  bottomRight,
}

class _PlotBounds {
  List<double> min;
  List<double> max;

  _PlotBounds({required this.min, required this.max});

  (double, double) get x => (min[0], max[0]);

  (double, double) get y => (min[1], max[1]);
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
    intervalDraw(
      canvas,
      size,
      bounds.y,
      yNumberToPixel,
      (n, y, paint) => drawText(
        doubleToString(n),
        Offset(size.width / 2 + 2.5, y),
        canvas,
        size,
        textStyle: TextStyle(
          fontSize: 12,
          color: paint.color,
        ),
        positionMode: _PositionMode.bottomLeft,
      ),
    );

    intervalDraw(
      canvas,
      size,
      bounds.x,
      xNumberToPixel,
      (n, x, paint) {
        // skip n = 0, since the vertical draw pass from above did that already
        if (n == 0) return;
        drawText(
          doubleToString(n),
          Offset(x + 2.5, size.height / 2),
          canvas,
          size,
          textStyle: TextStyle(
            fontSize: 12,
            color: paint.color,
          ),
          positionMode: _PositionMode.bottomLeft,
        );
      },
    );
  }

  void intervalDraw(
    Canvas canvas,
    Size size,
    (double, double) bounds,
    double Function(double, Size) numberToPixelFn,
    void Function(double, double, Paint) draw,
  ) {
    var (min, max) = bounds;
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
      map(x, bounds.min[0], bounds.max[0], 0, size.width);

  double yNumberToPixel(double y, Size size) =>
      map(y, bounds.min[1], bounds.max[1], 0, size.height);

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
    switch (positionMode) {
      case _PositionMode.bottomLeft:
        pos = Offset(pos.dx, pos.dy - painter.size.height);
        break;
      case _PositionMode.bottomRight:
        pos = Offset(pos.dx - painter.size.width, pos.dy - painter.size.height);
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
  late _PlotBounds bounds;

  @override
  void initState() {
    super.initState();
    var mediaQuery = (context
            .getElementForInheritedWidgetOfExactType<MediaQuery>()!
            .widget as MediaQuery)
        .data;
    var aspectRatio = mediaQuery.size.height / mediaQuery.size.width;

    var minX = -12.0;
    var maxX = 12.0;
    var minY = minX * aspectRatio;
    var maxY = maxX * aspectRatio;

    bounds = _PlotBounds(min: [minX, minY], max: [maxX, maxY]);
  }

  @override
  Widget build(BuildContext context) {
    return CustomPaint(
      painter: _PlotPainter(
        bounds: bounds,
      ),
      size: Size.infinite,
    );
  }
}
