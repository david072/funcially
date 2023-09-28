import 'dart:ffi';

import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart';
import 'package:frontend/main.dart';
import 'package:frontend/util/util.dart';
import 'package:settings_ui/settings_ui.dart';

const dateDelimiters = ".,'/-";

enum DateFormat { dmy, mdy, ymd }

extension ToString on DateFormat {
  String asString() {
    switch (this) {
      case DateFormat.dmy:
        return "DMY";
      case DateFormat.mdy:
        return "MDY";
      case DateFormat.ymd:
        return "YMD";
    }
  }

  String formatDate(DateTime date, String delimiter) {
    var parts = <int>[];
    switch (this) {
      case DateFormat.dmy:
        parts.addAll([date.day, date.month, date.year]);
        break;
      case DateFormat.mdy:
        parts.addAll([date.month, date.day, date.year]);
        break;
      case DateFormat.ymd:
        parts.addAll([date.year, date.month, date.day]);
        break;
    }

    return parts.map((i) => "$i".padLeft(2, '0')).join(delimiter);
  }
}

DateFormat dateFormatFromString(String s) {
  switch (s.toLowerCase()) {
    case "dmy":
      return DateFormat.dmy;
    case "mdy":
      return DateFormat.mdy;
    case "ymd":
      return DateFormat.ymd;
    default:
      return DateFormat.dmy;
  }
}

class SettingsPage extends StatefulWidget {
  const SettingsPage({
    super.key,
    required this.calculator,
    required this.monospaceTextStyle,
  });

  final int calculator;
  final TextStyle monospaceTextStyle;

  @override
  State<SettingsPage> createState() => _SettingsPageState();
}

class _SettingsPageState extends State<SettingsPage> {
  DateFormat dateFormat = DateFormat.dmy;
  String dateDelimiter = ".";

  @override
  void initState() {
    super.initState();
    getSettings();
  }

  void getSettings() {
    var settings = bindings.get_settings(widget.calculator);
    dateFormat =
        dateFormatFromString(settings.date.format.cast<Utf8>().toDartString());
    dateDelimiter = String.fromCharCode(settings.date.delimiter);
    bindings.free_settings(settings);
    setState(() {});
  }

  void saveSettings() {
    var settings = calloc<Settings>();
    settings.ref.date.format =
        dateFormat.asString().toNativeUtf8().cast<Char>();
    settings.ref.date.delimiter = dateDelimiter[0].codeUnits[0];

    bindings.set_settings(widget.calculator, settings.ref);
    calloc.free(settings);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Settings")),
      body: SettingsList(
        platform: DevicePlatform.android,
        applicationType: ApplicationType.material,
        sections: [
          SettingsSection(
            title: const Text("General"),
            tiles: [
              SettingsTile.switchTile(
                initialValue: true,
                onToggle: (_) {},
                title: const Text("Use thousands separator"),
                description: const Text(
                  "Format result numbers with a thousands separator, e.g. 1_000_000",
                ),
              ),
              SettingsTile.navigation(
                title: const Text("Date Format"),
                description: Text(
                    "Current format: ${dateFormat.formatDate(DateTime.now(), dateDelimiter)}"),
                onPressed: (context) => showDialog(
                  context: context,
                  builder: (context) => AlertDialog(
                    title: const Text("Date Format"),
                    content: StatefulBuilder(
                      builder: (context, builderSetState) {
                        void innerSetState(void Function() fn) {
                          builderSetState(() => setState(fn));
                        }

                        return Column(
                          mainAxisSize: MainAxisSize.min,
                          mainAxisAlignment: MainAxisAlignment.start,
                          crossAxisAlignment: CrossAxisAlignment.start,
                          children: [
                            LabeledDropdownButton(
                              labelText: "Format",
                              value: dateFormat,
                              hint: const Text("Format"),
                              onChanged: (value) {
                                if (value == null) return;
                                innerSetState(() => dateFormat = value);
                              },
                              items: DateFormat.values
                                  .map((fmt) => DropdownMenuItem(
                                        value: fmt,
                                        child: Text(fmt.asString()),
                                      ))
                                  .toList(),
                            ),
                            LabeledDropdownButton(
                              labelText: "Delimiter",
                              value: dateDelimiter,
                              hint: const Text("Delimiter"),
                              style: widget.monospaceTextStyle,
                              onChanged: (value) {
                                if (value == null) return;
                                innerSetState(() => dateDelimiter = value);
                              },
                              items: dateDelimiters.characters
                                  .map((del) => DropdownMenuItem(
                                        value: del,
                                        child: Text(del),
                                      ))
                                  .toList(),
                            ),
                          ],
                        );
                      },
                    ),
                  ),
                ).then((_) => saveSettings()),
              ),
            ],
          ),
        ],
      ),
    );
  }
}
