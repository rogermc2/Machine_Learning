--  Based on scikit-learn/sklearn/tree/_export.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with IL_Types;
--  with Printing;
with Weights;

package body Node_Strings is
   procedure Write_Decision_Criteria
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Data   : Tree.Tree_Node;
      Node_String : in out Unbounded_String);
   procedure Write_Impurity (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
                             Show_Labels : Boolean;
                             Node_Data   : Tree.Tree_Node;
                             Criteria    : Criterion.Classifier_Criteria_Type;
                             Node_String : in out Unbounded_String);
   procedure Write_Node_Class_Value
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs   : Tree.Tree_Cursor;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String);
   procedure Write_Node_Majority_Class
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String);
   procedure Write_Node_Samples_Count
     (Exporter            : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs, Top_Node : Tree.Tree_Cursor;
      Show_Labels         : Boolean;
      Node_String         : in out Unbounded_String);

   --  -------------------------------------------------------------------------
   --  L269 Node_To_String generates the node content string
   function Node_To_String
     (Exporter  : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs : Tree.Tree_Cursor;
      Criteria  : Criterion.Classifier_Criteria_Type) return String is
      use Tree;
      use Tree.Nodes_Package;
      --          Routine_Name    : constant String :=
      --                              "Node_Strings.Node_To_String ";
      Node_ID         : constant Positive := Element (Node_Curs).Node_ID;
      Node_ID_S       : constant String := Integer'Image (Node_ID);
      Top_Node        : constant Tree.Tree_Cursor :=
                          First_Child (Exporter.theTree.Nodes.Root);
      Node_Data       : constant Tree.Tree_Node := Element (Node_Curs);
      --  L277
      Show_Labels     : constant Boolean
        := Exporter.Label = To_Unbounded_String ("all") or
        (Exporter.Label = To_Unbounded_String ("root") and Node_ID = 1);
      Node_String     : Unbounded_String := To_Unbounded_String ("");

   begin
      --  L283
      if Exporter.Node_Ids then
         --  Write node ID
         if Show_Labels then
            Node_String := Node_String & "node ";
         end if;
         Node_String := Node_String & "#" & Node_ID_S & "\n ";
      end if;

      --  L289
      if not Element (Node_Curs).Leaf_Node then
         Write_Decision_Criteria (Exporter, Node_Data, Node_String);
      end if;

      if Exporter.Impurity then
         Write_Impurity (Exporter, Show_Labels, Node_Data, Criteria,
                         Node_String);
      end if;

      Write_Node_Samples_Count (Exporter, Node_Curs, Top_Node, Show_Labels,
                                Node_String);
      Write_Node_Class_Value (Exporter, Node_Curs, Node_ID, Show_Labels,
                              Node_String);

      --  L357
      if not Exporter.Class_Names.Is_Empty then
         Write_Node_Majority_Class (Exporter, Node_ID, Show_Labels,
                                    Node_String);
      end if;

      return To_String (Node_String);

   end Node_To_String;

   --  -------------------------------------------------------------------------

   procedure Write_Decision_Criteria
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Data   : Tree.Tree_Node;
      Node_String : in out Unbounded_String) is
      use IL_Types;
      use String_Vector_Package;
      --        Routine_Name     : constant String :=
      --                             "Node_Strings.Write_Decision_Criteria ";
      Feature_ID       : constant Positive := Node_Data.Best_Fit_Feature_Index;
      Feature_Names    : constant String_Vector := Exporter.Feature_Names;
      Feature          : Unbounded_String;
   begin
      if not Exporter.Feature_Names.Is_Empty then
         --  L294
         Feature := To_Unbounded_String ("X[") &
           Feature_Names.Element (Feature_ID) & "]";
      else
         Feature := To_Unbounded_String ("X[") &
           Integer'Image (Feature_ID) & "]";
      end if;

      --  L299
      Node_String := Node_String & Feature & " <= " &
        Classifier_Utilities.Float_Precision
        (Node_Data.Threshold, Exporter.Precision) & "\n ";
   end Write_Decision_Criteria;

   --  ----------------------------------------------------------------------

   procedure Write_Impurity (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
                             Show_Labels : Boolean;
                             Node_Data   : Tree.Tree_Node;
                             Criteria    : Criterion.Classifier_Criteria_Type;
                             Node_String : in out Unbounded_String) is
      use Criterion;
      Criteria_String : Unbounded_String := To_Unbounded_String ("");
   begin
      --  308  Write impurity
      if Show_Labels then
         case Criteria is
            when Gini_Criteria =>
               Criteria_String := To_Unbounded_String ("Gini");
            when Entropy_Criteria =>
               Criteria_String := To_Unbounded_String ("Entropy");
         end case;
         Node_String := Node_String & Criteria_String & " = ";
      end if;

      Node_String := Node_String & Classifier_Utilities.Float_Precision
        (Node_Data.Impurity, Exporter.Precision) & "\n ";
   end Write_Impurity;

   --  ----------------------------------------------------------------------

   procedure Write_Node_Class_Value
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs   : Tree.Tree_Cursor;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String) is
      use Ada.Containers;
      use IL_Types;
      use Tree;
      use Tree.Nodes_Package;
      Routine_Name    : constant String :=
                          "Node_Strings.Write_Node_Class_Value ";
      Node_Data       : constant Tree.Tree_Node := Element (Node_Curs);
      Classes         : constant IL_Types.Integer_List :=
                          Exporter.theTree.Classes;
      Class_Value     : Float;
      Value_First     : Boolean := True;
      Value_Text      : Unbounded_String := To_Unbounded_String ("");
      --  Values 2D array, num_nodes x num_classes per node.
      --  Values: dimension num_classes
      Values          : Weights.Weight_List :=
                          Exporter.theTree.Values.Element (Node_ID);
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "Exporter.theTree.Classes is empty");

      --  L331 Write distribution / regression value
      if Exporter.Proportion and Classes.Length /= 1 then
         for class_index in Values.First_Index ..
           Values.Last_Index loop
            Class_Value := Values.Element (class_index);
            --  L334
            Values.Replace_Element
              (class_index, Class_Value /
                 Float (Node_Data.Num_Node_Samples));
         end loop;
      end if;

      --  L335
      if Show_Labels then
         Node_String := Node_String & "value = ";
      end if;

      if Exporter.theTree.Num_Classes = 1 then
         --  L339 Regression--
         Value_Text :=
           To_Unbounded_String
             (Classifier_Utilities.Float_Precision
                (Values.Element (1), Exporter.Precision));
         --        elsif Exporter.Proportion then
         --           --   L342 Classification
         --           Value_Text :=
         --             To_Unbounded_String
         --               (Classifier_Utilities.Float_Precision & ", " &
         --                (Values.Element (1), Exporter.Precision));
      else
         --  L346
         Value_First := True;
         Value_Text := Value_Text & "[";
         for class_index in Values.First_Index ..
           Values.Last_Index loop
            if Values.Element (class_index) >
              10.0 ** (-Exporter.Precision) then
               if Value_First then
                  Value_First := False;
               else
                  Value_Text := Value_Text & ", ";
               end if;
               Value_Text := Value_Text &
               (To_Unbounded_String (Classifier_Utilities.Float_Precision
                (Values.Element (class_index), Exporter.Precision)));
            end if;
         end loop;
         Value_Text := Value_Text & "]";

         --           if output_index /= Value.Last_Index then
         --              Value_Text := Value_Text & "\n";
         --           end if;
      end if;

      Node_String := Node_String & "[" & Value_Text & "]" & "\n ";

   end Write_Node_Class_Value;

   --  -------------------------------------------------------------------------

   procedure Write_Node_Majority_Class
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String) is
      use Ada.Characters.Handling;
      --        Routine_Name    : constant String :=
      --                            "Node_Strings.Write_Node_Majority_Class ";
      Value           : constant Weights.Weight_List :=
                          Exporter.theTree.Values.Element (Node_ID);
      Arg_Max         : Positive;
      Class_Name      : Unbounded_String;
   begin
      --  L364
      if Show_Labels then
         Node_String := Node_String & "class = ";
      end if;

      Arg_Max := Weights.Max (Value);

      --  L366
      if Integer (Exporter.Class_Names.Length) = 1 and then
        To_Upper (To_String (Exporter.Class_Names.Element (1))) = "TRUE" then
         Class_Name := "y[" &
           To_Unbounded_String (Integer'Image (Arg_Max)) & "]";
      else
         Class_Name := Exporter.Class_Names.Element (Arg_Max);
      end if;
      Node_String := Node_String & Class_Name;

   end Write_Node_Majority_Class;

   --  -------------------------------------------------------------------------

   procedure Write_Node_Samples_Count
     (Exporter            : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs, Top_Node : Tree.Tree_Cursor;
      Show_Labels         : Boolean;
      Node_String         : in out Unbounded_String) is
      use Tree.Nodes_Package;
      Node_Data : constant Tree.Tree_Node := Element (Node_Curs);
      Percent   : Float;
   begin
      --  L320
      if Show_Labels then
         Node_String := Node_String & "samples = ";
      end if;

      if Exporter.Proportion then
         Percent := 100.0 * Float (Node_Data.Num_Node_Samples) /
           Float (Element (Top_Node).Num_Node_Samples);
         Node_String := Node_String &
           Classifier_Utilities.Float_Precision (Percent, 1) & "%" & "\n ";
      else
         Node_String := Node_String &
           Integer'Image (Node_Data.Num_Node_Samples) & "\n ";
      end if;
   end Write_Node_Samples_Count;

   --  ----------------------------------------------------------------------

end Node_Strings;
