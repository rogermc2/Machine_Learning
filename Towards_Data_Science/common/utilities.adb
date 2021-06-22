
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Utilities is

   use ML_Types;

   type Value_Data (Feature_Kind : Data_Type := Integer_Type) is record
      Feature_Name : Feature_Name_Type;
      case Feature_Kind is
         when Integer_Type => Integer_Value     : Integer;
         when Float_Type => Float_Value         : Float;
         when Boolean_Type => Boolean_Value     : Boolean;
         when UB_String_Type => UB_String_Value : Unbounded_String;
      end case;
   end record;

   package Values_Package is new Ada.Containers.Vectors
     (Class_Range, Value_Data);
   subtype Value_Set is Values_Package.Vector;

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
     (Classification : Count_Package.Map) is
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

   function Print_Leaf (Counts : Count_Package.Map) return String is
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

   procedure Print_Question (Message : String; Self : ML_Types.Question_Data) is
      Col          : constant String := To_String (Self.Feature_Name);
      Feature_Kind : constant Data_Type := Self.Feature_Kind;
   begin
      Put_Line (Message & " question:");
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

   procedure Print_Raw_Question (Self : Raw_Question) is
      --  Example" Self = ("Colour", "Green"));
      Col   : constant String := To_String (Self.Feature_Name);
      Value : constant String := To_String (Self.Feature_Value);
   begin
      Put_Line ("Raw_Question: Is " & Col & " = " & " " & Value);
   end Print_Raw_Question;

   --  --------------------------------------------------------------------------

   procedure Print_Rows (Message : String; Rows : Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message);
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

   procedure Print_Tree (aTree : Tree_Type) is
      use Tree_Package;
      --          Tree_Curs  : Tree_Cursor := aTree.Root;
      Level : Integer := 0;
      procedure Print_Node (Curs : Cursor) is
         Node : constant Tree_Node_Type := Element (Curs);
      begin
         Level := Level + 1;
         Put_Line (" Level: " & Integer'Image (Level));
         if Is_Leaf  (Curs) then
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
            Print_Node (First_Child (Curs));
            Print_Node (Last_Child (Curs));
--              Print_Rows ("True_Rows", Node.True_Rows);
--              Print_Rows ("False_Rows", Node.False_Rows);
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
      Print_Node (First_Child (aTree.Root));
--        aTree.Iterate (Print_Node'Access);
   end Print_Tree;

   --  ---------------------------------------------------------------------------

   procedure Print_UB_Class_Counts (Rows : Rows_Vector) is
      use UB_Label_Map_Package;
      Counts       : constant UB_Label_Map :=
                       Builder.UB_Class_Counts (Rows);
      Count_Cursor : UB_Label_Map_Package.Cursor := Counts.First;
      aCount       : Natural;
   begin
      Put_Line ("Class_Counts:");
      while Has_Element (Count_Cursor) loop
         aCount := Element (Count_Cursor);
         Put_Line (To_String ((Key (Count_Cursor))) &  ": " &
                     Natural'Image (aCount));
         next (Count_Cursor);
      end loop;
   end Print_UB_Class_Counts;

   --  --------------------------------------------------------------------------

   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Name_Type) is
      use Values_Package;
      Values : constant Value_Set := Unique_Values (Rows, Feature);
      Curs   : Cursor := Values.First;
      Data   : Value_Data;
   begin
      Put (To_String (Feature) & " Values:");
      while Has_Element (Curs) loop
         Data := Element (Curs);
         case Data.Feature_Kind is
            when Boolean_Type =>
               Put (" " & Boolean'Image (Data.Boolean_Value));
            when Float_Type =>
               Put (" " & Float'Image (Data.Float_Value));
            when Integer_Type =>
               Put (" " & Integer'Image (Data.Integer_Value));
            when UB_String_Type =>
               Put (" " & To_String (Data.UB_String_Value));
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
      use Values_Package;
      Data              : Row_Data := Rows.First_Element;
      Row2              : constant Row_Data :=
                            Rows.Element (Positive'Succ (Rows.First_Index));
      Num_Features      : constant Class_Range := Data.Class_Count;
      Row2_Features     : constant Feature_Data_Array (1 .. Num_Features) :=
                            Row2.Features;
      Feature_Name      : Feature_Name_Type;
      Feature_Data_Type : Data_Type;
      Value_String      : Unbounded_String;
      theSet            : Value_Set;

      procedure Add_To_Set (Value : Value_Data) is
      begin
         if not theSet.Contains (Value) then
         theSet.Append (Value);
      end if;
   end Add_To_Set;
begin

   if Rows.Length < 2 then
      raise Utilities_Exception with
        "Utilities.Unique_Values called with empty rows vector";
   else
      for index in Class_Range range
        Class_Range'Succ (Class_Range (Rows.First_Index)) ..
          Class_Range (Rows.Last_Index) loop
         Data := Rows.Element (Integer (index));
         for col in Class_Range range
           1 .. Num_Features loop
            Feature_Name :=
              Feature_Name_Type (Rows.First_Element.Features (col));
            if Feature_Name = Feature then
               Feature_Data_Type :=
                 Get_Data_Type (Row2_Features (col));
               Value_String := Data.Features (col);
               case Feature_Data_Type is
                  when Boolean_Type =>
                     declare
                        Feature_Value : Value_Data (Boolean_Type);
                     begin
                        Feature_Value.Feature_Name := Feature;
                        Feature_Value.Boolean_Value :=
                          Boolean'Value (To_String (Value_String));
                        Add_To_Set (Feature_Value);
                     end;

                  when Float_Type =>
                     declare
                        Feature_Value : Value_Data (Float_Type);
                     begin
                        Feature_Value.Feature_Name := Feature;
                        Feature_Value.Float_Value :=
                          Float'Value (To_String (Value_String));
                        Add_To_Set (Feature_Value);
                     end;

                  when Integer_Type =>
                     declare
                        Feature_Value : Value_Data (Integer_Type);
                     begin
                        Feature_Value.Feature_Name := Feature;
                        Feature_Value.Integer_Value :=
                          Integer'Value (To_String (Value_String));
                        Add_To_Set (Feature_Value);
                     end;

                  when UB_String_Type =>
                     declare
                        Feature_Value : Value_Data (UB_String_Type);
                     begin
                        Feature_Value.Feature_Name := Feature;
                        Feature_Value.UB_String_Value := Value_String;
                        Add_To_Set (Feature_Value);
                     end;
               end case;
            end if;
         end loop;
      end loop;
   end if;
   return theSet;
end Unique_Values;

--  --------------------------------------------------------------------------

end Utilities;
