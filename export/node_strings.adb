
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with ML_Types;
with Printing;
with Weights;

package body Node_Strings is
   procedure Write_Decision_Criteria
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive;
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
--           not Element (First_Child (Node_Curs)).Leaf_Node then
         Write_Decision_Criteria (Exporter, Node_ID, Node_Data, Node_String);
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
      if not Exporter.Class_Names.Is_Empty and then
        Integer (Exporter.theTree.Num_Outputs) = 1 and then
        Exporter.theTree.Num_Classes.Element (1) /= 1 then
         Write_Node_Majority_Class (Exporter, Node_ID, Show_Labels,
                                    Node_String);
      end if;

      return To_String (Node_String);

   end Node_To_String;

   --  -------------------------------------------------------------------------

   procedure Write_Decision_Criteria
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive;
      Node_Data   : Tree.Tree_Node;
      Node_String : in out Unbounded_String) is
      Routine_Name     : constant String :=
                           "Node_Strings.Write_Decision_Criteria ";
      Feature_ID       : Positive;
      Feature          : Unbounded_String;
      Feature_Names    : constant ML_Types.Feature_Names_List :=
                           Exporter.Feature_Names;
   begin
      if not Exporter.Feature_Names.Is_Empty then
         --  L294
         Printing.Print_Unbounded_List (Routine_Name & "Feature_Names",
                                        Exporter.Feature_Names);
         Feature := To_Unbounded_String ("X[") &
           Feature_Names.Element (Node_ID) & "]";
      else
         Feature_ID := Node_Data.Best_Fit_Feature_Index;
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
      --           if Show_Labels then
      --              Node_String := Node_String & "impurity = ";
      --           end if;

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
      use ML_Types;
      use Tree;
      use Tree.Nodes_Package;
      Routine_Name    : constant String :=
                          "Node_Strings.Write_Node_Class_Value ";
      Node_Data       : constant Tree.Tree_Node := Element (Node_Curs);
      Classes         : constant ML_Types.Value_Data_Lists_2D :=
                          Exporter.theTree.Classes;
      Class_Value     : Float;
      Value_First     : Boolean := True;
      Value_Text      : Unbounded_String := To_Unbounded_String ("");
      --  Value: num_outputs x num_classes
      Value           : Weights.Weight_Lists_2D :=
                          Exporter.theTree.Values.Element (Node_ID);
      Output_Data     : Weights.Weight_List;
   begin
      Assert (not Classes.Is_Empty, Routine_Name &
                "Exporter.theTree.Classes is empty");

      --  L331 Write distribution / regression value
      if Exporter.Proportion and Classes.Element (1).Length /= 1 then
         for output_index in Value.First_Index .. Value.Last_Index loop
            Output_Data := Value.Element (output_index);
            for class_index in Output_Data.First_Index ..
              Output_Data.Last_Index loop
               Class_Value := Output_Data.Element (class_index);
               --  L334
               Output_Data.Replace_Element
                 (class_index, Class_Value /
                    Float (Node_Data.Num_Node_Samples));
            end loop;
            Value.Replace_Element (output_index, Output_Data);
         end loop;
      end if;

      --  L335
      if Show_Labels then
         Node_String := Node_String & "value = ";
      end if;

      if Exporter.theTree.Num_Classes.Element (1) = 1 then
         --  L339 Regression--
         Value_Text :=
           To_Unbounded_String
             (Classifier_Utilities.Float_Precision
                (Output_Data.Element (1), Exporter.Precision) & ", " &
                Classifier_Utilities.Float_Precision
                (Output_Data.Element (2), Exporter.Precision));
      elsif Exporter.Proportion then
         --   L342 Classification
         Value_Text :=
           To_Unbounded_String
             (Classifier_Utilities.Float_Precision
                (Output_Data.Element (1), Exporter.Precision) & ", " &
                Classifier_Utilities.Float_Precision
                (Output_Data.Element (2), Exporter.Precision));
      else
         --  L346
         for output_index in Value.First_Index .. Value.Last_Index loop
            Output_Data := Value.Element (output_index);
            Value_First := True;
            Value_Text := Value_Text & "[";
            for class_index in Output_Data.First_Index ..
              Output_Data.Last_Index loop
               if Value_First then
                  Value_First := False;
               else
                  Value_Text := Value_Text & ", ";
               end if;
               Value_Text := Value_Text &
               (To_Unbounded_String (Classifier_Utilities.Float_Precision
                (Output_Data.Element (class_index), Exporter.Precision)));
            end loop;
            Value_Text := Value_Text & "]";

            if output_index /= Value.Last_Index then
               Value_Text := Value_Text & "\n";
            end if;
         end loop;
      end if;

      Node_String := Node_String & "[" & Value_Text & "]" & "\n ";

   end Write_Node_Class_Value;

   --  -------------------------------------------------------------------------

   procedure Write_Node_Majority_Class
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String) is
      Routine_Name    : constant String :=
                          "Node_Strings.Write_Node_Majority_Class ";
      --  Value: num_outputs x num_classes
      Value           : constant Weights.Weight_Lists_2D :=
                          Exporter.theTree.Values.Element (Node_ID);
      Output_Data     : Weights.Weight_List;
      Arg_Max         : Positive;
      Class_Name      : Unbounded_String;
   begin
      --  L364
      if Show_Labels then
         Node_String := Node_String & "class = ";
      end if;

      --  Calculate Arg_Max
      if Integer (Exporter.theTree.Num_Outputs) = 1 then
         Output_Data := Value.Element (1);
      else
         Put_Line (Routine_Name & "Num_Outputs > 1 not implemented");
      end if;
      Arg_Max := Weights.Max (Output_Data);

      --  L366
      if Exporter.Class_Names.Is_Empty then
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
