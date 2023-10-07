import 'dart:math';

import 'package:flutter/material.dart';
import 'package:frontend/widgets/plot.dart';

class PlotPage extends StatefulWidget {
  const PlotPage({super.key});

  @override
  State<PlotPage> createState() => _PlotPageState();
}

class _PlotPageState extends State<PlotPage> {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Plot")),
      body: const PlotWidget(
        graphs: [
          PlotGraph(
            name: "sin",
            function: sin,
            color: Colors.pink,
          ),
          PlotGraph(
            name: "cos",
            function: cos,
            color: Colors.green,
          ),
        ],
      ),
    );
  }
}
