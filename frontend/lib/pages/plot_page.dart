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
      body: const PlotWidget(),
    );
  }
}
