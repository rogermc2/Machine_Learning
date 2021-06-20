
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Utilities is

   use ML_Types;

   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Name_Type) return Value_Set;

   --  --------------------------------------------------------------------------

   function Get_Data_Type (Data : Unbounded_String) return ML_Types.Data_Type is
      theType : Data_Type;
   begin
      if Is_Integer (Data) then
         theType := Integer_Type;
      elsif Is_Float (Data) then
         theType := Float_Type;
      elsif Is_Boolean (Data) then
         theType := Boolean_Type;
      else
         theType := UB_String_Type;
      end if;

      return theType;
   end Get_Data_Type;

   --  ---------------------------------------------------------------------------

   function Is_Boolean (Item : in Unbounded_String) return Boolean is
      Item_String : constant String :=
                      Ada.Characters.Handling.To_Upper (To_String (Item));
   begin
      return Item_String = "TRUE" or else Item_String = "FALSE";
   end Is_Boolean;

   --  ---------------------------------------------------------------------------

   function Is_Float (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      use Ada.Strings;
   begin
      return Fixed.Count (Item_String, ".") = 1;
   end Is_Float;

   --  ---------------------------------------------------------------------------

   function Is_Integer (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      Dig         : Boolean := True;
   begin
      for index in Item_String'Range loop
         Dig := Dig and then
           Ada.Characters.Handling.Is_Decimal_Digit (Item_String (index));
      end loop;
      return Dig;
   end Is_Integer;

   --  ---------------------------------------------------------------------------

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

   procedure Print_Question (Label : String; Self : ML_Types.Question_Data) is
      Col          : constant String := To_String (Self.Feature_Name);
      Feature_Kind : constant Data_Type := Self.Feature_Kind;
   begin
      Put_Line (Label & " question:");
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

   procedure Print_Tree (aTree : ML_Types.Tree_Type) is
      use Tree_Package;
      --          Tree_Curs  : Tree_Cursor := aTree.Root;
      procedure Print_Node (Curs : Cursor) is
         Node : constant Decision_Node_Type := Element (Curs);
      begin
         if Is_Leaf  (Curs) then
            New_Line;
            Put ("Leaf node");
            Put_Line (" prediction: " & Natural'Image
                      (Node.Predictions.First_Element));
         else
            --           Put_Line ("Depth:" & Integer'Image (Integer (Depth (Curs))) & ", ");
            --                  case Node.Node_Type is
            --                  when  Decision_Kind =>
            Put ("Is " & To_String (Node.Question.Feature_Name) & " >= ");
            case Node.Question.Feature_Kind is
               when Integer_Type =>
                  Put (Integer'Image (Node.Question.Integer_Value));
               when Float_Type =>
                  Put (Float'Image (Node.Question.Float_Value));
               when Boolean_Type =>
                  Put (Boolean'Image (Node.Question.Boolean_Value));
               when UB_String_Type =>
                  Put (To_String (Node.Question.UB_String_Value));
            end case;
            Put_Line ("?");
            Print_Rows ("True_Rows", Node.True_Rows);
            Print_Rows ("False_Rows", Node.False_Rows);
            --                  when Prediction_Kind =>
            --                      Put_Line ("Prediction; " & Natural'Image
            --                                (Node.Predictions.First_Element));
            --                  end case;
         end if;
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

   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Name_Type) is
      use Value_Set_Package;
      Values : constant Value_Set := Unique_Values (Rows, Feature);
      Curs   : Cursor := Values.First;
      Data   : Value_Data (Feature);
   begin
      Put ("Unique " & Feature_Type'Image (Feature)  & " Values:");
      while Has_Element (Curs) loop
         case Feature is
            when Colour_Feature =>
               Data := Element (Curs);
               Put (" " & Colour_Type'Image (Data.Colour));
            when Diameter_Feature  =>
               Data := Element (Curs);
               Put (Integer'Image (Data.Diameter) & " ");
         end case;
         Next (Curs);
      end loop;
      New_Line;
   end Print_Unique_Values;

   --  -----------------------------------------------------------------------

   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Name_Type) return Value_Set is
      use Ada.Containers;
      use Rows_Package;
      Data           : Row_Data := Rows.First_Element;
      Row2           : constant Row_Data :=
                                Rows.Element (Positive'Succ (Rows.First_Index));
      Num_Features      : constant Class_Range := Data.Class_Count;
      Row2_Features     : constant Feature_Data_Array (1 .. Num_Features) :=
                                Row2.Features;
      Feature_Name      : Feature_Name_Type;
      Feature_Data_Type : Data_Type;
      Value             : Value_Data (Feature);
      theSet            : Value_Set;
   begin

      if Rows.Length < 2 then
         raise Utilities_Exception with
           "Utilities.Unique_Values called with empty rows vector";
      else
         for index in Rows.First_Index .. Rows.Last_Index loop
            Data := Rows.Element (index);
            Feature_Name :=
              Feature_Name_Type (Rows.First_Element.Features (Class_Range (index)));
            Feature_Data_Type := Get_Data_Type (Row2_Features (Class_Range (index)));
            if Feature = Colour_Feature then
               Value.Colour := Data.Colour;
            else
               Value.Diameter := Data.Diameter;
            end if;
            if not theSet.Contains (Value) then
               theSet.Append (Value);
            end if;
         end loop;
      end if;
      return theSet;
   end Unique_Values;

   --  --------------------------------------------------------------------------

end Utilities;
