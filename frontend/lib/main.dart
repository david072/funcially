/*
 * Copyright (c) 2023, david072
 *
 * SPDX-License-Identifier: Apache-2.0
 */

import 'dart:ffi' hide Size;
import 'dart:io';

import 'package:flutter/material.dart';
import 'package:frontend/calculator_bindings.dart' hide Color, ColorSegment;
import 'package:frontend/pages/home_page.dart';

const String _libName = "dart_bridge";

final DynamicLibrary _dylib = () {
  if (Platform.isMacOS || Platform.isIOS) {
    return DynamicLibrary.executable();
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

const String spUseThousandsSeparator = "settings_use-thousands-separator";
const String spDateFormat = "settings_date-format";
const String spDateDelimiter = "settings_date-delimiter";

void main() {
  runApp(MaterialApp(
    debugShowCheckedModeBanner: false,
    home: const HomePage(),
    themeMode: ThemeMode.dark,
    theme: ThemeData.dark(useMaterial3: true),
  ));
}
