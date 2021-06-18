
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Utilities is

   use ML_Types;

   --     procedure Print_Row (Label : String; Row : ML_Types.Row_Data);

   --  --------------------------------------------------------------------------

   procedure Print_Best (Self : Builder.Best_Data) is
      Question     : constant Question_Data :=
                       Builder.Best_Question (Self);
      Feature      : constant String := To_String (Question.Feature_Name);
      Feature_Kind : constant Data_Type := Question.Feature_Kind;
   begin
      New_Line;
      Put_Line ("Best question:");
      Put (Feature & " = ");
      case Feature_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Question.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Question.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Question.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Question.UB_String_Value));
      end case;
      Put_Line ("Gain = " & Float'Image (Builder.Gain (Self)));

   end Print_Best;

   --  --------------------------------------------------------------------------

   procedure Print_Classification
     (Classification : ML_Types.Count_Package.Map) is
      use Count_Package;
      aCount : Natural;
   begin
      Put_Line ("Classification:");
      for index in Classification.First_Key .. Classification.Last_Key loop
         if Classification.Contains (index) then
            aCount := Classification.Element (index);
            Put_Line (Data_Type'Image (index) &  ": " & Natural'Image (aCount));
         else
            Put_Line (Data_Type'Image (index) &  ": none");
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("Print_Classification exception");
         raise;
   end Print_Classification;

   --  --------------------------------------------------------------------------

    function Print_Leaf (Counts : ML_Types.Count_Package.Map) return String is
        use Count_Package;
        Total         : Natural := 0;
        aCount        : Natural;
        aString       : Unbounded_String;
        Prob          : Natural;
    begin
        Put_Line ("Counts size:" & Natural'Image (Natural (Counts.Length)));
        for index in Counts.First_Key .. Counts.Last_Key loop
            if Counts.Contains (index) then
                Total := Total + Counts.Element (index);
                --              Put_Line ("Total:" & Natural'Image (Total));
            end if;
        end loop;

        --        Put_Line ("Probabilities:");
        for index in Counts.First_Key .. Counts.Last_Key loop
            if Counts.Contains (index) then
                aCount := Counts.Element (index);
                --              Put_Line ("aCount:" & Natural'Image (aCount));
                Prob := (100 * aCount) / Total;
                aString :=
                  To_Unbounded_String (Natural'Image (Prob) & "%");
                --              Probabilities.Replace (index, Prob);
                --              Put_Line (To_String (aString));
            end if;
        end loop;
        return To_String (aString);
    end Print_Leaf;

    --  --------------------------------------------------------------------------

   procedure Print_Question (Self : ML_Types.Question_Data) is
      Col          : constant String := To_String (Self.Feature_Name);
      Feature_Kind : constant Data_Type := Self.Feature_Kind;
   begin
      Put_Line ("Question data:");
      Put ("Feature " & Col & " = ");
      case Feature_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Self.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Self.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Self.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Self.UB_String_Value));
      end case;
      Put_Line (Float'Image (Self.Gain));

   end Print_Question;

   --  --------------------------------------------------------------------------

   procedure Print_Raw_Question (Self : ML_Types.Raw_Question) is
      --  Example" Self = ("Colour", "Green"));
      Col   : constant String := To_String (Self.Feature_Name);
      Value : constant String := To_String (Self.Feature_Value);
   begin
      Put_Line ("Raw_Question: Is " & Col & " = " & " " & Value);
   end Print_Raw_Question;

   --  --------------------------------------------------------------------------

   --      procedure Print_Row (Label : String; Row : ML_Types.Row_Data) is
   --      begin
   --          Put (Label);
   --          Put (": (");
   --          for feat in Row.Features'First .. Row.Features'Last (1) loop
   --              Put (To_String (Row.Features (feat)));
   --          end loop;
   --          Put (") " & To_String (Row.Label));
   --          New_Line;
   --      end Print_Row;

   --  --------------------------------------------------------------------------

   procedure Print_Rows (Label : String; Rows : ML_Types.Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Label);
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("(");
         for feat in aRow.Features'First .. aRow.Features'Last (1) loop
            Put (To_String (aRow.Features (feat)));
         end loop;
         Put (") " & To_String (aRow.Label));
         if index /= Rows.Last_Index then
            Put (", ");
         end if;
      end loop;
      New_Line;
   end Print_Rows;

   --  --------------------------------------------------------------------------

   procedure Print_Tree (aTree : ML_Types.Tree_Package.Tree) is
      use Tree_Package;
      procedure Print_Node (Curs : Cursor) is
         Node : constant Decision_Node_Type := Element (Curs);
      begin
         if Is_Leaf  (Curs) then
            New_Line;
            Put_Line ("Leaf node");
         end if;
         Put_Line ("Depth:" & Integer'Image (Integer (Depth (Curs))) & ", ");
         case Node.Node_Type is
            when  Decision_Kind =>
               --              Print_Question (Node.Question);
               Put_Line ("Decision:");
               Print_Rows ("True Rows", Node.True_Rows);
               Print_Rows ("False Rows", Node.False_Rows);
            when Prediction_Kind =>
               Put_Line ("Prediction; " & Natural'Image
                         (Node.Predictions.First_Element));
         end case;
      end Print_Node;
   begin
      --  Iterate calls Print_Node.all with a cursor that designates each
      --  element in aTree starting with the root node and proceeding in a
      --  depth-first order.
      Put_Line ("Depth first tree traversal");
      aTree.Iterate (Print_Node'Access);
   end Print_Tree;

   --  ---------------------------------------------------------------------------

   procedure Print_UB_Class_Counts (Rows : ML_Types.Rows_Vector) is
      use UB_Label_Map_Package;
      Counts       : constant UB_Label_Map :=
                       Builder.UB_Class_Counts (Rows);
      Count_Cursor : UB_Label_Map_Package.Cursor := Counts.First;
      aCount       : Natural;
   begin
      Put_Line ("Class_Counts:");
      while Has_Element (Count_Cursor) loop
         aCount := Element (Count_Cursor);
         Put_Line (To_String ((Key (Count_Cursor))) &  ": " & Natural'Image (aCount));
         next (Count_Cursor);
      end loop;
   end Print_UB_Class_Counts;

   --  --------------------------------------------------------------------------

end Utilities;
