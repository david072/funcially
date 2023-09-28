import 'dart:ffi';

import 'package:ffi/ffi.dart';
import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart';
import 'package:frontend/main.dart';
import 'package:frontend/util/util.dart';
import 'package:settings_ui/settings_ui.dart';
import 'package:shared_preferences/shared_preferences.dart';

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

class CalculatorSettings {
  bool useThousandsSeparator;
  DateFormat dateFormat;
  String dateDelimiter;

  CalculatorSettings({
    required this.useThousandsSeparator,
    required this.dateFormat,
    required this.dateDelimiter,
  });

  static Future<CalculatorSettings> load() async {
    var sp = await SharedPreferences.getInstance();
    var useThousandsSeparator = sp.getBool(spUseThousandsSeparator) ?? true;
    var dateFormat = dateFormatFromString(sp.getString(spDateFormat) ?? "");
    var dateDelimiter = sp.getString(spDateDelimiter) ?? ".";

    return CalculatorSettings(
      useThousandsSeparator: useThousandsSeparator,
      dateFormat: dateFormat,
      dateDelimiter: dateDelimiter,
    );
  }

  Future<void> save({int? toCalculator}) async {
    if (toCalculator != null) saveSettingsToCalculator(toCalculator);

    var sp = await SharedPreferences.getInstance();
    sp.setBool(spUseThousandsSeparator, useThousandsSeparator);
    sp.setString(spDateFormat, dateFormat.asString());
    sp.setString(spDateDelimiter, dateDelimiter);
  }

  void saveSettingsToCalculator(int calculator) {
    var settings = calloc<Settings>();
    settings.ref.date.format =
        dateFormat.asString().toNativeUtf8().cast<Char>();
    settings.ref.date.delimiter = dateDelimiter[0].codeUnits[0];

    bindings.set_settings(calculator, settings.ref);
    calloc.free(settings);
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
  bool loading = true;
  late CalculatorSettings settings;

  @override
  void initState() {
    super.initState();
    CalculatorSettings.load().then((value) {
      settings = value;
      setState(() => loading = false);
    });
  }

  void saveSettings() => settings.save(toCalculator: widget.calculator);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: const Text("Settings")),
      body: !loading
          ? SettingsList(
              platform: DevicePlatform.android,
              applicationType: ApplicationType.material,
              sections: [
                SettingsSection(
                  title: const Text("General"),
                  tiles: [
                    SettingsTile.switchTile(
                      initialValue: settings.useThousandsSeparator,
                      onToggle: (value) {
                        setState(() => settings.useThousandsSeparator = value);
                        saveSettings();
                      },
                      title: const Text("Use thousands separator"),
                      description: const Text(
                        "Format result numbers with a thousands separator, e.g. 1_000_000",
                      ),
                    ),
                    SettingsTile.navigation(
                      title: const Text("Date Format"),
                      description: Text(
                          "Current format: ${settings.dateFormat.formatDate(DateTime.now(), settings.dateDelimiter)}"),
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
                                    value: settings.dateFormat,
                                    hint: const Text("Format"),
                                    onChanged: (value) {
                                      if (value == null) return;
                                      innerSetState(
                                          () => settings.dateFormat = value);
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
                                    value: settings.dateDelimiter,
                                    hint: const Text("Delimiter"),
                                    style: widget.monospaceTextStyle,
                                    onChanged: (value) {
                                      if (value == null) return;
                                      innerSetState(
                                          () => settings.dateDelimiter = value);
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
            )
          : const Center(child: CircularProgressIndicator()),
    );
  }
}
