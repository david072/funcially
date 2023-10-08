import 'package:flutter/material.dart';

class MenuEntry {
  const MenuEntry({
    required this.label,
    this.shortcut,
    this.onPressed,
    this.children,
  });

  final String label;
  final MenuSerializableShortcut? shortcut;
  final VoidCallback? onPressed;
  final List<MenuEntry>? children;

  static List<Widget> build(List<MenuEntry> entries) => entries
      .map((e) => e.children != null
          ? SubmenuButton(
              menuChildren: build(e.children!),
              child: Text(e.label),
            )
          : MenuItemButton(
              shortcut: e.shortcut,
              onPressed: e.onPressed,
              child: Text(e.label),
            ))
      .toList();

  static Map<MenuSerializableShortcut, Intent> shortcuts(
      List<MenuEntry> entries) {
    var result = <MenuSerializableShortcut, Intent>{};
    for (var entry in entries) {
      if (entry.children != null) {
        result.addAll(shortcuts(entry.children!));
      } else if (entry.shortcut != null && entry.onPressed != null) {
        result[entry.shortcut!] = VoidCallbackIntent(entry.onPressed!);
      }
    }

    return result;
  }
}
