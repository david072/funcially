import 'dart:math' as math;

import 'package:flutter/material.dart';

double log10(num x) => math.log(x) / math.ln10;

double map(double n, double a1, double b1, double a2, double b2) =>
    (n - a1) * (b2 - a2) / (b1 - a1) + a2;

double roundDp(double n, int dp) {
  var multiplier = math.pow(10, dp);
  return (n * multiplier).roundToDouble() / multiplier;
}

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

  void scale(double factor, double focalPoint) {
    min = (min - focalPoint) / factor + focalPoint;
    max = (max - focalPoint) / factor + focalPoint;
  }

  double numberToPixel(double n, double maxSize, {bool invert = false}) =>
      map(n, min, max, 0, maxSize);

  double pixelToNumber(double px, double maxSize) =>
      map(px, 0, maxSize, min, max);

  @override
  bool operator ==(Object other) =>
      other is _AxisBounds &&
      other.runtimeType == runtimeType &&
      other.min == min &&
      other.max == max;

  @override
  int get hashCode => Object.hash(min, max);
}

class _PlotBounds {
  _AxisBounds x;
  _AxisBounds y;

  _PlotBounds({required this.x, required this.y});

  @override
  bool operator ==(Object other) =>
      other is _PlotBounds && other.x == x && other.y == y;

  @override
  int get hashCode => Object.hash(x, y);
}

class _Intervals {
  final double smallInterval;
  final double smallPower;
  final double baseInterval;
  final double basePower;

  _Intervals({
    required this.smallInterval,
    required this.baseInterval,
    required this.smallPower,
    required this.basePower,
  });

  _Intervals min(_Intervals other) {
    return _Intervals(
      smallInterval: math.min(other.smallInterval, smallInterval),
      smallPower: math.min(other.smallPower, smallPower),
      baseInterval: math.min(other.baseInterval, baseInterval),
      basePower: math.min(other.basePower, basePower),
    );
  }
}

class _PlotPainter extends CustomPainter {
  static const baseColor = Colors.grey;
  static const minLargeIntervalDistance = 10.0;
  static const maxLargeIntervalDistance = 50.0;

  final _PlotBounds bounds;
  final List<PlotGraph> graphs;

  _PlotPainter({
    required this.bounds,
    this.graphs = const [],
  });

  Paint get baseIntervalPaint => Paint()
    ..color = baseColor
    ..strokeWidth = 1;

  @override
  void paint(Canvas canvas, Size size) {
    var xIntervals = calculateIntervals(bounds.x);
    var yIntervals = calculateIntervals(bounds.y);
    var intervals = xIntervals.min(yIntervals);
    drawGrid(canvas, size, intervals);
    drawLabels(canvas, size, intervals);

    for (var graph in graphs) {
      drawGraph(canvas, size, intervals, graph);
    }
  }

  void drawGraph(
    Canvas canvas,
    Size size,
    _Intervals intervals,
    PlotGraph graph,
  ) {
    var paint = Paint()
      ..color = graph.color
      ..strokeWidth = 1;
    var points = graph._generatePoints(
        bounds.x, math.pow(10, intervals.smallPower - 1) as double);
    Offset? lastPoint;
    for (var point in points) {
      var (x, y) = (
        xNumberToPixel(point.$1, size),
        yNumberToPixel(point.$2, size),
      );
      if (lastPoint == null) {
        lastPoint = Offset(x, y);
        continue;
      }

      canvas.drawLine(lastPoint, Offset(x, y), paint);
      lastPoint = Offset(x, y);
    }
  }

  void drawGrid(Canvas canvas, Size size, _Intervals intervals) {
    intervalDraw(
      canvas,
      size,
      bounds.y,
      intervals,
      yNumberToPixel,
      (n, y, paint) =>
          canvas.drawLine(Offset(0, y), Offset(size.width, y), paint),
    );

    intervalDraw(
      canvas,
      size,
      bounds.x,
      intervals,
      xNumberToPixel,
      (_, x, paint) =>
          canvas.drawLine(Offset(x, 0), Offset(x, size.height), paint),
    );
  }

  void drawLabels(Canvas canvas, Size size, _Intervals intervals) {
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
      intervals,
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
      xAxisPosition = size.height;
    } else {
      xAxisPosition = 0;
    }

    intervalDraw(
      canvas,
      size,
      bounds.x,
      intervals,
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
    _Intervals intervals,
    double Function(double, Size) numberToPixelFn,
    void Function(double, double, Paint) draw,
  ) {
    var (min, max) = bounds.record();

    var smallIntervalPaint =
        getSmallIntervalPaint(intervals.smallInterval, size, numberToPixelFn);

    void drawInterval(double interval, double power, double min, Paint paint) {
      var startOffset = calculateStartOffset(interval, min);
      for (double n = startOffset + min; n <= max; n += interval) {
        var px = numberToPixelFn(n, size);
        int dp = power < 0 ? power.abs().toInt() : 0;
        var nRounded = roundDp(n, dp);
        if (nRounded == -0.0) nRounded = 0;
        draw(nRounded, px, paint);
      }
    }

    drawInterval(
        intervals.smallInterval, intervals.smallPower, min, smallIntervalPaint);
    drawInterval(
        intervals.baseInterval, intervals.basePower, min, baseIntervalPaint);
  }

  _Intervals calculateIntervals(_AxisBounds bounds) {
    // log10(interval length) gives us the magnitude of the numbers in the range
    // i.e. [-12; 12] would have a length of 24, and floor(log10(24)) = 1
    // our step size is then 10 to the power of that, meaning 10^1.
    var power = log10(bounds.length).floorToDouble();
    var smallInterval = math.pow(10.0, power - 1) as double;
    var baseInterval = math.pow(10.0, power) as double;
    return _Intervals(
      baseInterval: baseInterval,
      basePower: power,
      smallInterval: smallInterval,
      smallPower: power - 1,
    );
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
    var diff = p2 > p1 ? p2 - p1 : p1 - p2;
    var distanceBetweenLargeIntervalTicks =
        diff.clamp(minLargeIntervalDistance, maxLargeIntervalDistance);

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
      bounds.x.numberToPixel(x, size.width);

  double yNumberToPixel(double y, Size size) =>
      size.height - bounds.y.numberToPixel(y, size.height);

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
  bool shouldRepaint(_PlotPainter oldPainter) =>
      oldPainter.bounds != bounds || oldPainter.graphs != graphs;
}

class PlotGraph {
  final Color color;
  final double Function(double x) function;

  const PlotGraph({
    required this.function,
    this.color = Colors.white,
  });

  List<(double, double)> _generatePoints(
    _AxisBounds xRange,
    double pointOffset,
  ) {
    List<(double, double)> result = [];
    for (double x = xRange.min; x <= xRange.max; x += pointOffset) {
      result.add((x, function(x)));
    }

    return result;
  }

  @override
  bool operator ==(Object other) =>
      other is PlotGraph && other.color == color && other.function == function;

  @override
  int get hashCode => Object.hash(color, function);
}

class PlotWidget extends StatefulWidget {
  const PlotWidget({
    super.key,
    this.graphs = const [],
  });

  final List<PlotGraph> graphs;

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
    bounds!.y.min += diff;
    bounds!.y.max += diff;

    previousScalePos = details.localFocalPoint;
  }

  void scaleBounds(ScaleUpdateDetails details, BoxConstraints constraints) {
    var scalingFactor = details.scale / lastScalingFactor;
    bounds!.x.scale(
      scalingFactor,
      bounds!.x.pixelToNumber(details.localFocalPoint.dx, constraints.maxWidth),
    );
    bounds!.y.scale(
      scalingFactor,
      bounds!.y.pixelToNumber(
          constraints.maxHeight - details.localFocalPoint.dy,
          constraints.maxHeight),
    );
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
            graphs: widget.graphs,
          ),
          size: Size.infinite,
        ),
      );
    });
  }
}
