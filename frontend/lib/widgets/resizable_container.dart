import 'package:flutter/material.dart';

class ResizableContainer extends StatefulWidget {
  const ResizableContainer({
    super.key,
    required this.initialWidth,
    this.borderColor = const Color(0xFF777777),
    this.borderHighlightColor = const Color(0xFFe8e8e8),
    this.borderWidth = 2,
    this.decoration,
    this.color,
    this.padding,
    this.child,
  });

  final double initialWidth;
  final Color borderColor;
  final Color borderHighlightColor;
  final double borderWidth;
  final Decoration? decoration;
  final Color? color;
  final EdgeInsetsGeometry? padding;
  final Widget? child;

  @override
  State<ResizableContainer> createState() => _ResizableContainerState();
}

class _ResizableContainerState extends State<ResizableContainer> {
  late double width;
  bool isDragged = false;

  @override
  void initState() {
    super.initState();
    width = widget.initialWidth;
  }

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        Container(
          decoration: widget.decoration,
          color: widget.color,
          margin: EdgeInsets.only(left: (widget.borderWidth * 10) / 2),
          padding: widget.padding,
          height: double.infinity,
          width: width,
          child: widget.child,
        ),
        GestureDetector(
          onPanStart: (_) => setState(() => isDragged = true),
          onPanUpdate: (details) => setState(() => width -= details.delta.dx),
          onPanEnd: (_) => setState(() => isDragged = false),
          child: Container(
            width: widget.borderWidth * 10,
            color: Colors.transparent,
            child: Align(
              child: Container(
                color: !isDragged
                    ? widget.borderColor
                    : widget.borderHighlightColor,
                width: widget.borderWidth,
                height: double.infinity,
              ),
            ),
          ),
        ),
      ],
    );
  }
}
