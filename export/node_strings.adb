
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with ML_Types;
with Weights;

package body Node_Strings is

   procedure Write_Node_Class_Value
     (Exporter  : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs : Tree.Tree_Cursor;
      Node_ID   : Positive; Show_Labels : Boolean);
   procedure Write_Node_Majority_Class
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String);

   --  -------------------------------------------------------------------------
   --  L269 Node_To_String generates the node content string
   function Node_To_String
     (Exporter  : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs : Tree.Tree_Cursor;
      Criteria  : Criterion.Classifier_Criteria_Type) return String is
      use Criterion;
      use ML_Types;
      use Tree;
      use Tree.Nodes_Package;
      Routine_Name    : constant String :=
                          "Graphviz_Exporter.Node_To_String ";
      Node_ID         : constant Positive := Element (Node_Curs).Node_ID;
      Node_ID_S       : constant String := Integer'Image (Node_ID);
      Top_Node        : constant Tree.Tree_Cursor :=
                          First_Child (Exporter.theTree.Nodes.Root);
      Node_Data       : constant Tree.Tree_Node := Element (Node_Curs);
      Classes         : constant ML_Types.Value_Data_Lists_2D :=
                          Exporter.theTree.Classes;
      --  L277
      Show_Labels     : constant Boolean
        := Exporter.Label = To_Unbounded_String ("all") or
        (Exporter.Label = To_Unbounded_String ("root") and Node_ID = 1);
      Feature_Names   : constant ML_Types.Feature_Names_List :=
                          Exporter.Feature_Names;
      Feature         : Unbounded_String;
      Percent         : Float;
      Node_String     : Unbounded_String := To_Unbounded_String ("");
      Criteria_String : Unbounded_String := To_Unbounded_String ("");

      procedure Write_Decision_Criteria is
      begin
         if Exporter.Feature_Names.Is_Empty then
            --  L294
            Feature := To_Unbounded_String ("X[") &
              Feature_Names.Element (Node_ID) & "]";
            Put_Line (Routine_Name & "Feature_Names is empty");
         else
            if not Exporter.theTree.Features.Is_Empty then
               Feature := Exporter.theTree.Features.Element (Node_ID);
            end if;
         end if;

         --  L299
         Node_String := Node_String & Feature & " <= " &
           Classifier_Utilities.Float_Precision
           (Node_Data.Threshold, Exporter.Precision) & "\n";
      end Write_Decision_Criteria;

      --  ----------------------------------------------------------------------

      procedure Write_Impurity is
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
           (Node_Data.Impurity, Exporter.Precision) & "\n";
      end Write_Impurity;

      --  ----------------------------------------------------------------------

      procedure Write_Node_Samples_Count is
      begin
         --  L320
         if Show_Labels then
            Node_String := Node_String & "samples = ";
         end if;

         if Exporter.Proportion then
            Percent := 100.0 * Float (Node_Data.Num_Node_Samples) /
              Float (Element (Top_Node).Num_Node_Samples);
            Node_String := Node_String &
              Classifier_Utilities.Float_Precision (Percent, 1) & "%" & "\n";
         else
            Node_String := Node_String &
              Integer'Image (Node_Data.Num_Node_Samples) & "\n";
         end if;
      end Write_Node_Samples_Count;

      --  ----------------------------------------------------------------------

   begin
      --  L283
      if Exporter.Node_Ids then
         --  Write node ID
         if Show_Labels then
            Node_String := Node_String & "node ";
         end if;
         Node_String := Node_String & "#" & Node_ID_S & "\n";
      end if;

      --  L289
      if not Element (First_Child (Node_Curs)).Leaf_Node then
         --          if not Node_Data.Leaf_Node then
         Write_Decision_Criteria;
      end if;

      if Exporter.Impurity then
         Write_Impurity;
      end if;

      Write_Node_Samples_Count;
      Write_Node_Class_Value (Exporter, Node_Curs, Node_ID, Show_Labels);

      --  L357
      if not Exporter.Class_Names.Is_Empty and then
        Integer (Exporter.theTree.Num_Outputs) = 1 and then
        Classes.Element (1).Element (1).Integer_Value /= 1 then
         Write_Node_Majority_Class (Exporter, Node_ID, Show_Labels,
                                    Node_String);
      end if;

      return To_String (Node_String);

   end Node_To_String;

   --  -------------------------------------------------------------------------

   procedure Write_Node_Class_Value
     (Exporter  : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_Curs : Tree.Tree_Cursor;
      Node_ID   : Positive; Show_Labels : Boolean) is
      use Ada.Containers;
      use ML_Types;
      use Tree;
      use Tree.Nodes_Package;
      Node_Data       : constant Tree.Tree_Node := Element (Node_Curs);
      Classes         : constant ML_Types.Value_Data_Lists_2D :=
                          Exporter.theTree.Classes;
      Class_Value     : Float;
      Value_First     : Boolean := True;
      Value_Text      : Unbounded_String := To_Unbounded_String ("");
      Pos             : Natural;
      --  Value: num_outputs x num_classes
      Value           : Weights.Weight_Lists_2D :=
                          Exporter.theTree.Values.Element (Node_ID);
      Output_Data     : Weights.Weight_List;
      Node_String     : Unbounded_String := To_Unbounded_String ("");
   begin
      --  L331 Write distribution / regression value
      if Exporter.Proportion and Classes.Element (1).Length /= 1 then
         for output_index in Value.First_Index .. Value.Last_Index loop
            Output_Data := Value.Element (output_index);
            for class_index in Output_Data.First_Index ..
              Output_Data.Last_Index loop
               Class_Value := Output_Data.Element (class_index);
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

      --  L340
      if Exporter.Proportion then
         for output_index in Value.First_Index .. Value.Last_Index loop
            Output_Data := Value.Element (output_index);
            for class_index in Output_Data.First_Index ..
              Output_Data.Last_Index loop
               if Value_First then
                  Value_First := False;
               else
                  Value_Text := Value_Text & ", ";
               end if;
               --  L342
               Value_Text := Value_Text &
               (To_Unbounded_String (Classifier_Utilities.Float_Precision
                (Output_Data.Element (class_index), Exporter.Precision)));
            end loop;
         end loop;
      end if;

      --  L352
      if Integer (Exporter.theTree.Num_Outputs) = 1 and then
        Exporter.theTree.Classes.Element (1).Element (1).Value_Kind =
        Integer_Type then
         Pos := Index (Value_Text, "[");
         while Pos /= 0 loop
            Delete (Value_Text, Pos, Pos);
            Pos := Index (Value_Text, "[");
         end loop;
         Pos := Index (Value_Text, "]");
         if Pos > 0 then
            while Pos <= Length (Value_Text) loop
               Delete (Value_Text, Pos, Pos);
               Pos := Index (Value_Text, "]");
            end loop;
         end if;
      end if;
      Node_String := Node_String & "[" & Value_Text & "]" & "\n";

   end Write_Node_Class_Value;

   --  -------------------------------------------------------------------------

   procedure Write_Node_Majority_Class
     (Exporter    : Graphviz_Exporter.DOT_Tree_Exporter;
      Node_ID     : Positive; Show_Labels : Boolean;
      Node_String : in out Unbounded_String) is
      --  Value: num_outputs x num_classes
      Value           : constant Weights.Weight_Lists_2D :=
                          Exporter.theTree.Values.Element (Node_ID);
      Output_Data     : Weights.Weight_List;
      Arg_Max         : Positive;
      Class_Name      : Unbounded_String;
   begin
      if Show_Labels then
         Node_String := Node_String & "class = ";
      end if;

      Output_Data := Value.Element (Node_ID);
      Arg_Max := Weights.Max (Output_Data);

      --  L366
      if Exporter.Class_Names.Is_Empty then
         Class_Name := Exporter.Class_Names.Element (Arg_Max);
      else
         Class_Name := "y[" &
           To_Unbounded_String (Integer'Image (Arg_Max)) & "]";
      end if;
      Node_String := Node_String & Class_Name;

   end Write_Node_Majority_Class;

   --  -------------------------------------------------------------------------

end Node_Strings;
