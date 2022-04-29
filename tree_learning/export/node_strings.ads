
with Criterion;
with Graphviz_Exporter;
with Tree;

package Node_Strings is

  function Node_To_String
     (Exporter  : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs : Tree.Tree_Cursor;
      Criteria  : Criterion.Classifier_Criteria_Type) return String;

end Node_Strings;
