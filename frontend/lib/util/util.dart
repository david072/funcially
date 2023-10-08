import 'package:flutter/material.dart';

class TextFieldEditor {
  final TextEditingController controller;
  final UndoHistoryController undoController;
  final FocusNode focusNode;

  const TextFieldEditor({
    required this.controller,
    required this.undoController,
    required this.focusNode,
  });

  void targetRequestFocus() => focusNode.requestFocus();

  void insertText(String text) {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      var textBeforeCursor = controller.text.substring(0, sel.start);
      var textAfterCursor = controller.text.substring(sel.start);
      controller.text = textBeforeCursor + text + textAfterCursor;

      controller.selection = TextSelection.collapsed(
        offset: sel.start + text.length,
        affinity: sel.affinity,
      );
    } else {
      var textBeforeSelection = controller.text.substring(0, sel.start);
      var textAfterSelection = controller.text.substring(sel.end);
      controller.text = textBeforeSelection + text + textAfterSelection;

      controller.selection = TextSelection.collapsed(
        offset: sel.start + text.length,
        affinity: TextAffinity.downstream,
      );
    }
  }

  void insertFunction(String name) {
    insertText("$name()");
    var sel = controller.selection;
    controller.selection = TextSelection.collapsed(
      offset: sel.start - 1,
      affinity: sel.affinity,
    );
  }

  void backspace() {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      if (sel.start == 0) return;
      var textBefore = controller.text.substring(0, sel.start - 1);
      var textAfter = controller.text.substring(sel.start);
      controller.text = textBefore + textAfter;

      controller.selection = TextSelection.collapsed(
        offset: sel.start - 1,
        affinity: sel.affinity,
      );
    } else {
      var textBeforeSelection = controller.text.substring(0, sel.start);
      var textAfterSelection = controller.text.substring(sel.end);
      controller.text = textBeforeSelection + textAfterSelection;

      controller.selection = TextSelection.collapsed(
        offset: sel.start,
        affinity: TextAffinity.downstream,
      );
    }
  }

  void moveSelectionHorizontally(int offset) {
    targetRequestFocus();
    var sel = controller.selection;
    if (sel.isCollapsed) {
      var newOffset = sel.start + offset;
      if (newOffset < 0 || newOffset > controller.text.length) return;
      sel = TextSelection.collapsed(
        offset: newOffset,
        affinity: sel.affinity,
      );
    } else {
      var newOffset = sel.extentOffset + offset;
      if (newOffset < 0 || newOffset > controller.text.length) return;
      sel = TextSelection(
        baseOffset: sel.baseOffset,
        extentOffset: sel.extentOffset + offset,
      );
    }

    controller.selection = sel;
  }
}

class LabeledDropdownButton<T> extends StatelessWidget {
  const LabeledDropdownButton({
    super.key,
    required this.labelText,
    required this.value,
    required this.onChanged,
    required this.items,
    this.hint,
    this.style,
  });

  final String labelText;
  final T value;
  final Widget? hint;
  final TextStyle? style;
  final void Function(T?) onChanged;
  final List<DropdownMenuItem<T>> items;

  @override
  Widget build(BuildContext context) {
    return Row(
      children: [
        Text(
          labelText,
          style: Theme.of(context).textTheme.bodyLarge,
        ),
        const Spacer(),
        DropdownButton<T>(
          value: value,
          hint: hint,
          style: style,
          onChanged: onChanged,
          items: items,
        ),
      ],
    );
  }
}

String longestLine(String s) {
  var lines = s.split("\n");
  var linesSorted = lines.indexed.map((e) => (e.$1, e.$2.length)).toList()
    ..sort((a, b) => a.$2.compareTo(b.$2));
  return lines[linesSorted.last.$1];
}

Size textDimensions(String text, TextStyle style) {
  var painter = TextPainter(
      text: TextSpan(text: text, style: style),
      maxLines: null,
      textDirection: TextDirection.ltr)
    ..layout(minWidth: 0, maxWidth: double.infinity);
  return painter.size;
}

Widget wrapInScrollView({
  required BuildContext context,
  required Widget widget,
  bool reverse = false,
  double? minWidth,
}) =>
    SingleChildScrollView(
      scrollDirection: Axis.horizontal,
      reverse: reverse,
      child: IntrinsicWidth(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            ConstrainedBox(
              constraints: BoxConstraints(
                minWidth: minWidth ?? MediaQuery.of(context).size.width,
                maxHeight: 0,
              ),
              child: const SizedBox.expand(),
            ),
            Expanded(child: widget),
          ],
        ),
      ),
    );

TextField bareTextField({
  TextEditingController? controller,
  ScrollController? scrollController,
  TextStyle? textStyle,
  void Function(String)? onChanged,
  String? hintText,
  bool readOnly = false,
  TextAlign textAlign = TextAlign.start,
  TextInputType? textInputType,
  bool autofocus = false,
  FocusNode? focusNode,
  UndoHistoryController? undoController,
}) =>
    TextField(
      decoration: InputDecoration(
        border: InputBorder.none,
        hintText: hintText,
        isDense: true,
        constraints: const BoxConstraints(),
        contentPadding: EdgeInsets.zero,
      ),
      controller: controller,
      scrollController: scrollController,
      undoController: undoController,
      autofocus: autofocus,
      focusNode: focusNode,
      readOnly: readOnly,
      keyboardType: textInputType,
      textAlign: textAlign,
      spellCheckConfiguration: const SpellCheckConfiguration.disabled(),
      expands: true,
      style: textStyle,
      maxLines: null,
      autocorrect: false,
      enableSuggestions: false,
      onChanged: onChanged,
    );

extension MoreInteratorFunction<T> on Iterable<T> {
  T? firstWhereOrNull(bool Function(T element) predicate) {
    for (var t in this) {
      if (predicate(t)) return t;
    }

    return null;
  }
}

extension IfNotNull<T> on T? {
  R? map<R>(R Function(T it) predicate) {
    if (this == null) return null;
    return predicate(this as T);
  }
}
