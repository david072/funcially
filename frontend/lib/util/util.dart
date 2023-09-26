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
