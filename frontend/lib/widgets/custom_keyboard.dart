import 'package:flutter/material.dart';
import 'package:frontend/util/util.dart';

enum KeyboardType {
  numbers,
  functions,
  normal,
}

extension ToString on KeyboardType {
  String asString() {
    switch (this) {
      case KeyboardType.numbers:
        return "123";
      case KeyboardType.functions:
        return "f(x)";
      case KeyboardType.normal:
        return "ABC";
    }
  }
}

Widget keyboardButton({
  required Widget child,
  void Function()? onPressed,
  bool emphasize = false,
}) =>
    Padding(
      padding: const EdgeInsets.all(2),
      child: ElevatedButton(
        style: ElevatedButton.styleFrom(
          elevation: 0,
          backgroundColor: Colors.grey.withOpacity(emphasize ? .15 : .05),
          shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(5)),
          padding: EdgeInsets.zero,
          shadowColor: Colors.transparent,
        ),
        onPressed: onPressed,
        child: child,
      ),
    );

class CalculatorKeyboard extends StatefulWidget {
  const CalculatorKeyboard({
    super.key,
    required this.targetController,
    required this.targetUndoController,
    required this.targetFocusNode,
  });

  final TextEditingController targetController;
  final UndoHistoryController targetUndoController;
  final FocusNode targetFocusNode;

  @override
  State<CalculatorKeyboard> createState() => CalculatorKeyboardState();
}

class CalculatorKeyboardState extends State<CalculatorKeyboard> {
  static const Map<KeyboardType, List<List<List<String>>>> layout = {
    KeyboardType.numbers: [
      [
        ["t#f", "t#g", "t#x", "t#y"],
        ["t#pi", "t#e", "f#sqrt", "empty"],
        ["t#^", "t#=", "t#<", "t#>"],
        ["t#in", "empty", "empty", "empty"],
        ["t#.", "t#,", "t#(", "t#)"],
      ],
      [
        ["t#:=", "t#/", "t#*", "t#-"],
        ["t#7", "t#8", "t#9", "t#+"],
        ["t#4", "t#5", "t#6", "backspace"],
        ["t#1", "t#2", "t#3", "return"],
        ["t#0", "space", "left", "right"],
      ],
    ],
    KeyboardType.functions: [
      [
        ["f#sin", "f#cos", "f#tan", "f#cot"],
        ["f#asin", "f#acos", "f#atan", "f#acot"],
        ["f#ln", "f#log", "f#cbrt", "f#root"],
        ["f#abs", "f#floor", "f#ceil", "f#round"],
        ["f#lerp", "f#clamp", "f#map", "empty"],
      ],
      [
        ["t#%", "t#of", "t#!", "t#mod"],
        ["t#&", "t#|", "t#<<", "t#>>"],
        ["t#{", "t#}", "t##", "backspace"],
        ["t#[", "t#]", "t#°", "return"],
        ["t#0b", "t#0x", "left", "right"],
      ],
    ],
  };

  KeyboardType currentKeyboard = KeyboardType.numbers;
  bool showKeyboard = true;

  late TextFieldEditor editor;

  @override
  void initState() {
    super.initState();
    editor = TextFieldEditor(
      controller: widget.targetController,
      undoController: widget.targetUndoController,
      focusNode: widget.targetFocusNode,
    );
  }

  void setShowKeyboard(bool show) => setState(() => showKeyboard = show);

  Widget keyboardLayoutForType(KeyboardType type) {
    // Form a column with multiple rows, with a separator to separate the blocks
    var rows = <Widget>[];

    for (int i = 0; i < layout[type]![0].length; i++) {
      var children = <Widget>[];
      for (int j = 0; j < layout[type]!.length; j++) {
        children.addAll(layout[type]![j][i]
            .map(keyToWidget)
            .map((e) => Expanded(child: e)));
        if (j != layout[type]!.length - 1) {
          children.add(const SizedBox(width: 10));
        }
      }

      rows.add(Expanded(
          child: Row(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: children,
      )));
    }

    return Expanded(
      child: Column(children: rows),
    );
  }

  Widget keyToWidget(String key) {
    var separatorIdx = key.indexOf("#");
    if (separatorIdx == -1) separatorIdx = key.length;
    var type = key.substring(0, separatorIdx);
    switch (type) {
      case "t":
        return keyboardButton(
          child: Text(key.substring(2)),
          onPressed: () => editor.insertText(key.substring(2)),
        );
      case "f":
        return keyboardButton(
          child: Text(key.substring(2)),
          onPressed: () => editor.insertFunction(key.substring(2)),
        );
      case "empty":
        return keyboardButton(child: const SizedBox());
      case "return":
        return keyboardButton(
          child: const Icon(Icons.keyboard_return_outlined),
          onPressed: () => editor.insertText("\n"),
          emphasize: true,
        );
      case "backspace":
        return keyboardButton(
          child: const Icon(Icons.backspace_outlined),
          onPressed: editor.backspace,
          emphasize: true,
        );
      case "space":
        return keyboardButton(
          child: const Icon(Icons.space_bar),
          onPressed: () => editor.insertText(" "),
          emphasize: true,
        );
      case "left":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_left_outlined),
          onPressed: () => editor.moveSelectionHorizontally(-1),
          emphasize: true,
        );
      case "right":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_right_outlined),
          onPressed: () => editor.moveSelectionHorizontally(1),
          emphasize: true,
        );
      default:
        throw "Invalid key type '$type'";
    }
  }

  Widget keyboardUi() {
    switch (currentKeyboard) {
      case KeyboardType.normal:
        return Expanded(
          child: Align(
            alignment: Alignment.bottomCenter,
            child: NormalKeyboardLayout(editor: editor),
          ),
        );
      default:
        return keyboardLayoutForType(currentKeyboard);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      width: double.infinity,
      height: showKeyboard ? MediaQuery.of(context).size.height / 2.8 : 68,
      color: Colors.grey.withOpacity(.05),
      padding: const EdgeInsets.all(10),
      child: Column(
        children: [
          Row(
            children: [
              if (showKeyboard)
                SegmentedButton(
                  segments: KeyboardType.values
                      .map((key) => ButtonSegment(
                            value: key,
                            label: Text(key.asString()),
                          ))
                      .toList(),
                  selected: {currentKeyboard},
                  showSelectedIcon: false,
                  onSelectionChanged: (newSelection) =>
                      setState(() => currentKeyboard = newSelection.first),
                  style: const ButtonStyle(
                    tapTargetSize: MaterialTapTargetSize.shrinkWrap,
                    visualDensity: VisualDensity(horizontal: -3, vertical: -3),
                  ),
                ),
              const Spacer(),
              ValueListenableBuilder(
                valueListenable: widget.targetUndoController,
                builder: (context, value, _) {
                  return IconButton(
                    icon: const Icon(Icons.undo),
                    onPressed:
                        value.canUndo ? widget.targetUndoController.undo : null,
                  );
                },
              ),
              ValueListenableBuilder(
                valueListenable: widget.targetUndoController,
                builder: (context, value, _) => IconButton(
                  icon: const Icon(Icons.redo),
                  onPressed:
                      value.canRedo ? widget.targetUndoController.redo : null,
                ),
              ),
              IconButton(
                icon:
                    Icon(showKeyboard ? Icons.unfold_less : Icons.unfold_more),
                onPressed: () => setState(() => showKeyboard = !showKeyboard),
              ),
            ],
          ),
          if (showKeyboard) keyboardUi(),
        ],
      ),
    );
  }
}

class NormalKeyboardLayout extends StatefulWidget {
  const NormalKeyboardLayout({super.key, required this.editor});

  final TextFieldEditor editor;

  @override
  State<NormalKeyboardLayout> createState() => _NormalKeyboardLayoutState();
}

class _NormalKeyboardLayoutState extends State<NormalKeyboardLayout> {
  static const List<List<String>> normalLayout = [
    ["q", "w", "e", "r", "t", "z", "u", "i", "o", "p"],
    ["#3", "a", "s", "d", "f", "g", "h", "j", "k", "l", "#3"],
    ["#shift", "y", "x", "c", "v", "b", "n", "m", "#backspace"],
    ["#switch", ",", ".", "#space", "#left", "#right", "#return"],
  ];

  static const List<List<String>> numberLayout = [
    ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"],
    ["@", "#", "€", "_", "&", "-", "+", "(", ")", "/"],
    ["#switch2", "*", "\"", "'", ":", ";", "!", "?", "#backspace"],
    ["#switch", ",", ".", "#space", "#left", "#right", "#return"],
  ];

  static const List<List<String>> symbolLayout = [
    ["~", "`", "|", "•", "√", "π", "÷", "×", "§", "Δ"],
    ["£", "¥", "\$", "¢", "^", "°", "=", "{", "}", "\\"],
    ["#switch2", "%", "©", "®", "™", "✓", "[", "]", "#backspace"],
    ["#switch", "<", ">", "#space", "#left", "#right", "#return"],
  ];

  bool isCaps = false;
  bool isInNumberLayout = false;
  bool isInSymbolLayout = false;

  Widget specialKeyTypeToWidget(String type) {
    switch (type) {
      case "shift":
        return keyboardButton(
          child: const Icon(Icons.arrow_upward_rounded),
          onPressed: () => setState(() => isCaps = !isCaps),
          emphasize: true,
        );
      case "backspace":
        return keyboardButton(
          child: const Icon(Icons.backspace_outlined),
          onPressed: widget.editor.backspace,
          emphasize: true,
        );
      case "switch":
        return keyboardButton(
          child: Text(!isInNumberLayout ? "?123" : "ABC"),
          onPressed: () {
            isInNumberLayout = !isInNumberLayout;
            if (!isInNumberLayout) isInSymbolLayout = false;
            setState(() {});
          },
          emphasize: true,
        );
      case "switch2":
        return keyboardButton(
          child: Text(!isInSymbolLayout ? "=\\<" : "?123"),
          onPressed: () => setState(() => isInSymbolLayout = !isInSymbolLayout),
          emphasize: true,
        );
      case "space":
        return keyboardButton(
          child: const Icon(Icons.space_bar),
          onPressed: () => widget.editor.insertText(" "),
        );
      case "left":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_left_outlined),
          onPressed: () => widget.editor.moveSelectionHorizontally(-1),
          emphasize: true,
        );
      case "right":
        return keyboardButton(
          child: const Icon(Icons.keyboard_arrow_right_outlined),
          onPressed: () => widget.editor.moveSelectionHorizontally(1),
          emphasize: true,
        );
      case "return":
        return keyboardButton(
          child: const Icon(Icons.keyboard_return_outlined),
          onPressed: () => widget.editor.insertText("\n"),
          emphasize: true,
        );
      default:
        throw "Unknown type '$type'";
    }
  }

  @override
  Widget build(BuildContext context) {
    var layout = !isInNumberLayout
        ? normalLayout
        : !isInSymbolLayout
            ? numberLayout
            : symbolLayout;

    return Column(
      mainAxisSize: MainAxisSize.min,
      children: layout
          .map((row) => Expanded(
                child: Row(
                  crossAxisAlignment: CrossAxisAlignment.stretch,
                  children: row.map((String key) {
                    if (!key.startsWith("#") || key.length < 2) {
                      var symbol =
                          !isCaps ? key.toLowerCase() : key.toUpperCase();
                      return Expanded(
                        child: keyboardButton(
                          child: Text(symbol),
                          onPressed: () => widget.editor.insertText(symbol),
                        ),
                      );
                    } else {
                      var keyType = key.substring(1);
                      var spacingMultiplier = int.tryParse(keyType);
                      if (spacingMultiplier != null) {
                        return SizedBox(width: 5.0 * spacingMultiplier);
                      } else {
                        return Expanded(child: specialKeyTypeToWidget(keyType));
                      }
                    }
                  }).toList(),
                ),
              ))
          .toList(),
    );
  }
}
