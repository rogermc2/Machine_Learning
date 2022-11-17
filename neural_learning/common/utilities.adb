
with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Strings.Fixed;

with Maths;

package body Utilities is

   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Name_Type) return Value_List;

   --  --------------------------------------------------------------------------

   procedure Check_Rows (Rows : in out Rows_Vector) is
      use Ada.Containers;
      use Rows_Package;
      Data          : Row_Data := Rows.First_Element;
      Num_Features  : constant Class_Range := Data.Class_Count;
      Feature_Types : Data_Type_Array (1 .. Num_Features);
      Label_Type    : Data_Type := Get_Data_Type (Data.Label);
      Data_Changed  : Boolean := False;
   begin
      if Rows.Length < 2 then
         raise Utilities_Exception with
           "Utilities.Check_Rows called with empty rows vector";
      else
         for index in 1 .. Num_Features loop
            Feature_Types (index) := Get_Data_Type (Data.Features (index));
         end loop;

         for row in
           Integer'Succ (Rows.First_Index) .. Rows.Last_Index loop
            Data := Rows.Element (Integer (row));
            Data_Changed := False;
            for col in Class_Range range 1 .. Num_Features loop
               if Get_Data_Type (Data.Features (col)) = Float_Type and then
                 Feature_Types (col) = Integer_Type then
                  Data.Features (col) := Data.Features (col) & ".0";
                  Feature_Types (col) := Float_Type;
                  Data_Changed := True;
               elsif
                 Get_Data_Type (Data.Features (col)) = Integer_Type and then
                 Feature_Types (col) = Float_Type then
                  Data.Features (col) := Data.Features (col) & ".0";
                  Data_Changed := True;
               end if;
            end loop;

            if Get_Data_Type (Data.Label) = Float_Type and then
              Label_Type = Integer_Type then
               Data.Label := Data.Label & ".0";
               Label_Type := Float_Type;
               Data_Changed := True;
            end if;
            if Data_Changed then
               Rows.Replace_Element (row, Data);
            end if;
         end loop;
      end if;

   end Check_Rows;

   --  --------------------------------------------------------------------------

   function Get_Column (List_2D      : Value_Data_Lists_2D;
                        Column_Index : Positive)
                        return Value_Data_List is
      aList  : Value_Data_List;
      Column : Value_Data_List;
      Data   : Value_Record;
   begin
      for index in List_2D.First_Index .. List_2D.Last_Index loop
         aList := List_2D.Element (index);
         Data := aList.Element (Column_Index);
         Column.Append (Data);
      end loop;

      return Column;

   end Get_Column;

   --  -------------------------------------------------------------------------

   function Get_Data_Type (Data : Unbounded_String) return Data_Type is
      theType   : Data_Type;
      aString   : constant String := To_String (Data);
      S_Last    : constant Integer := aString'Last;
      Last_Char : Character;
      UB_Data   : Unbounded_String := Data;
   begin
      Assert (aString'Length > 0,
              "Utilities.Get_Data_Type called with empty string");
      Last_Char := aString (S_Last);
      if Character'Pos (Last_Char) < 32 then
         UB_Data := To_Unbounded_String (aString (1 .. S_Last - 1));
      end if;

      if Is_Integer (UB_Data) then
         theType := Integer_Type;
      elsif Is_Float (UB_Data) then
         theType := Float_Type;
      elsif Is_Boolean (UB_Data) then
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

   --  -------------------------------------------------------------------------

   function Is_Float (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      use Ada.Strings;
   begin
      return Fixed.Count (Item_String, ".") = 1;
   end Is_Float;

   --  -------------------------------------------------------------------------

   function Is_Integer (Item : Unbounded_String) return Boolean is
      UB_String : Unbounded_String := Item;
      Dig       : Boolean := True;
   begin
      UB_String := Trim (UB_String, Ada.Strings.Left);
      UB_String := Trim (UB_String, Ada.Strings.Right);

      declare
         Item_String : constant String := To_String (UB_String);
      begin
         for index in Item_String'First .. Item_String'Last loop
            Dig := Dig and then
              (Ada.Characters.Handling.Is_Decimal_Digit
                 (Item_String (index)) or else
               Character'Pos (Item_String (index)) < 32);
         end loop;
      end;

      return Dig;

   end Is_Integer;

   --  ---------------------------------------------------------------------------

   function Load_Raw_CSV_Data (File_Name : String) return Raw_Data_Vector is
      Data_File : File_Type;
      Data      : Raw_Data_Vector;
   begin
      Open (Data_File, In_File, File_Name);
      Data := Load_Raw_CSV_Data (Data_File);
      Close (Data_File);

      return Data;

   end Load_Raw_CSV_Data;

   --  -------------------------------------------------------------------------

   function Load_Raw_CSV_Data (Data_File : File_Type)
                               return Raw_Data_Vector is
      use String_Package;
      Data_Line : Unbounded_String;
      CSV_Line  : String_List;
      Curs      : String_Package.Cursor;
      Values    : Unbounded_List;
      Data      : Raw_Data_Vector;
   begin
      while not End_Of_File (Data_File) loop
         Data_Line := To_Unbounded_String (Get_Line (Data_File));
         CSV_Line := Utilities.Split_String
           (To_String (Data_Line), ",");
         Curs := CSV_Line.First;
         Values.Clear;
         while Has_Element (Curs) loop
            Values.Append (Element (Curs));
            Next (Curs);
         end loop;
         Data.Append (Values);
      end loop;

      return Data;

   end Load_Raw_CSV_Data;

   --  -------------------------------------------------------------------------

   function Number_Of_Features (Rows : Rows_Vector) return Class_Range is
      Data  : constant Row_Data := Rows.First_Element;
   begin
      return Data.Class_Count;
   end Number_Of_Features;

   --  -------------------------------------------------------------------------

   function Number_Of_Features (Rows : Value_Data_List) return Class_Range is
   begin
      return Class_Range (Rows.Length);
   end Number_Of_Features;

   --  -------------------------------------------------------------------------

   function Pair_Items (a, b : Integer_Array) return Integer_Pair_List is
      Item   : Integer_Pair;
      Result : Integer_Pair_List;
   begin
      for index in a'First .. a'Last loop
         Item := (a (index), b (index));
         Result.Append (Item);
      end loop;

      return Result;

   end Pair_Items;

   --  --------------------------------------------------------------------------

   procedure Permute (anArray : in out NL_Arrays_And_Matrices.Float_Array) is
      Array_Length  : constant Positive := Positive (anArray'Length);
      Index_2       : Natural;
      Rand          : Positive;
   begin
      if Array_Length > 1 then
         for row in anArray'Range loop
            Rand := row +
              Natural (abs (Maths.Random_Float) * Float (Array_Length - row));
            Index_2 := 0;
            while Index_2 <= anArray'Last and then Index_2 < Rand loop
               Index_2 := Index_2 + 1;
            end loop;

            if Index_2 <= anArray'Last then
               Swap (anArray, row, Index_2);
            end if;

         end loop;
      end if;

   end Permute;

   --  -------------------------------------------------------------------------

   procedure Permute (anArray : in out Integer_Array) is
      Array_Length  : constant Positive := Positive (anArray'Length);
      Index_2       : Natural;
      Rand          : Positive;
   begin
      if Array_Length > 1 then
         for row in anArray'Range loop
            Rand := row +
              Natural (abs (Maths.Random_Float) * Float (Array_Length - row));
            Index_2 := 0;
            while Index_2 <= anArray'Last and then Index_2 < Rand loop
               Index_2 := Index_2 + 1;
            end loop;

            if Index_2 <= anArray'Last then
               Swap (anArray, row, Index_2);
            end if;

         end loop;
      end if;

   end Permute;

   --  -------------------------------------------------------------------------

   function Permute (aMatrix : Integer_Matrix) return Integer_Matrix is
      List_Length  : constant Positive := Positive (aMatrix'Length);
      Rand         : Positive;
      Permutation  : Integer_Matrix := aMatrix;
   begin
      if List_Length > 1 then
         for index in 1 .. List_Length - 1 loop
            Rand := index +
              Natural (abs (Maths.Random_Float) * Float (List_Length - index));
            Swap (Permutation, index, Rand);
         end loop;
      end if;

      return Permutation;

   end Permute;

   --  -------------------------------------------------------------------------

   procedure Permute (aList : in out String_List) is
      use String_Package;
      List_Length  : constant Positive := Positive (aList.Length);
      Curs_1       : Cursor := aList.First;
      Curs_2       : Cursor := aList.First;
      Rand         : Positive;
      Index        : Natural := 0;
      Index_2      : Natural := 0;
   begin
      if List_Length > 1 then
         while Has_Element (Curs_1) loop
            Index := Index + 1;
            Rand := index +
              Natural (abs (Maths.Random_Float) * Float (List_Length - index));
            Curs_2 := Next (Curs_1);
            Index_2 := 0;
            while Has_Element (Curs_2) and then Index_2 < Rand loop
               Index_2 := Index_2 + 1;
               Curs_2 := Next (Curs_2);
            end loop;

            if Has_Element (Curs_2) then
               Swap (aList, Curs_1, Curs_2);
            end if;
            Next (Curs_1);
         end loop;
      end if;

   end Permute;

   --  -------------------------------------------------------------------------

   function Permute (aMatrix :  ML_Types.Real_Float_Matrix)
                     return ML_Types.Real_Float_Matrix is
      List_Length  : constant Positive := Positive (aMatrix'Length);
      Rand         : Positive;
      Permutation  : ML_Types.Real_Float_Matrix := aMatrix;
   begin
      if List_Length > 1 then
         for index in 1 .. List_Length - 1 loop
            Rand := index +
              Natural (abs (Maths.Random_Float) * Float (List_Length - index));
            Swap (Permutation, index, Rand);
         end loop;
      end if;

      return Permutation;

   end Permute;

   --  -------------------------------------------------------------------------

   procedure Print_Value_Record (Message : String; Value : Value_Record) is
      Value_Kind : constant Data_Type := Value.Value_Kind;
   begin
      New_Line;
      Put_Line (Message & " " & Data_Type'Image (Value.Value_Kind) &
                  " value record:");
      Put ("  Value: ");
      case Value_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Value.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Value.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Value.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Value.UB_String_Value));
      end case;

   end Print_Value_Record;

   --  ------------------------------------------------------------------------

   --     function Prediction_String (Label_Counts : Predictions_List)
   --                                 return String is
   --        use Prediction_Data_Package;
   --        Count_Cursor : Cursor := Label_Counts.First;
   --        Prediction   : Prediction_Data;
   --        Total        : Natural := 0;
   --        Leaf_Data    : Unbounded_String := To_Unbounded_String
   --          ("{'");
   --     begin
   --        while Has_Element (Count_Cursor) loop
   --           Total := Total + Element (Count_Cursor).Num_Copies;
   --           Next (Count_Cursor);
   --        end loop;
   --        Count_Cursor := Label_Counts.First;
   --        while Has_Element (Count_Cursor) loop
   --           Prediction := Element (Count_Cursor);
   --           Leaf_Data := Leaf_Data & To_Unbounded_String
   --             (To_String (Prediction.Label) & "': '" &
   --                Integer'Image ((100 * Prediction.Num_Copies) / Total) &
   --                "%'");
   --           if Count_Cursor /= Label_Counts.Last then
   --              Leaf_Data := Leaf_Data & ", ";
   --           end if;
   --           Next (Count_Cursor);
   --        end loop;
   --        return To_String (Leaf_Data) & "}";
   --     end Prediction_String;

   --  -------------------------------------------------------------------------

   procedure Print_Feature_Values (Message : String; Rows : Rows_Vector;
                                   Column  : Class_Range) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message & ":");
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("  Feature value: ");
         Put (To_String (aRow.Features (Column)));
         if Column /= aRow.Features'Last then
            Put (", ");
         end if;
         New_Line;
      end loop;

   end Print_Feature_Values;

   --  ------------------------------------------------------------------------

   procedure Print_Feature_Types
     (Message : String; theTypes : Classifier_Utilities.Feature_Type_Array) is
      Count : Natural := 0;
   begin
      Put_Line (Message & " Feature types:");
      for index in theTypes'First .. theTypes'Last loop
         Count := Count + 1;
         Put (Data_Type'Image (theTypes (index)));
         if index /= theTypes'Last then
            Put (", ");
         end if;

         if index /= theTypes'Last and then Count > 5 then
            New_Line;
            Count := 0;
         end if;

      end loop;
      New_Line;

   end Print_Feature_Types;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Pairs (Message : String; Pairs : Float_Pair_List) is
      aPair : Float_Pair;
   begin
      Put_Line (Message);
      for index in Pairs.First_Index .. Pairs.Last_Index loop
         aPair := Pairs (index);
         Put ("(" & Float'Image (aPair.Float_1) & "," &
                Float'Image (aPair.Float_2) & ")");
         if index /= Pairs.Last_Index then
            Put (", ");
         end if;
      end loop;

   end Print_Float_Pairs;

   --  -------------------------------------------------------------------------

   procedure Print_Integer_Pairs (Message : String;
                                  Pairs   : Integer_Pair_List) is
      aPair : Integer_Pair;
   begin
      Put_Line (Message);
      for index in Pairs.First_Index .. Pairs.Last_Index loop
         aPair := Pairs (index);
         Put ("(" & Integer'Image (aPair.Integer_1) & "," &
                Integer'Image (aPair.Integer_2) & ")");
         if index /= Pairs.Last_Index then
            Put (", ");
         end if;
      end loop;
      New_Line;

   end Print_Integer_Pairs;

   --  -------------------------------------------------------------------------

   procedure Print_Label_Types
     (Message : String; theTypes : Classifier_Utilities.Label_Type_Array) is
      Count : Natural := 0;
   begin
      Put_Line (Message & " Label types:");
      for index in theTypes'First .. theTypes'Last loop
         Count := Count + 1;
         Put (Data_Type'Image (theTypes (index)));
         if index /= theTypes'Last then
            Put (", ");
         end if;

         if index /= theTypes'Last and then Count > 5 then
            New_Line;
            Count := 0;
         end if;

      end loop;
      New_Line;

   end Print_Label_Types;

   --  ------------------------------------------------------------------------

   procedure Print_Row (Message : String; aRow : Row_Data) is
   begin
      Put_Line (Message);
      Put ("  Feature values: ");
      for feat in aRow.Features'First .. aRow.Features'Last loop
         Put (To_String (aRow.Features (feat)));
         if feat /= aRow.Features'Last then
            Put (", ");
         end if;
      end loop;
      Put_Line ("; Label: " & To_String (aRow.Label));
   end Print_Row;

   --  ------------------------------------------------------------------------

   procedure Print_Row (Message    : String; Rows : Rows_Vector;
                        Row_Number : Positive) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message & ":");
      aRow := Rows.Element (Row_Number);
      Put ("  Feature values: (");
      for feat in aRow.Features'First .. aRow.Features'Last loop
         Put (To_String (aRow.Features (feat)));
         if feat /= aRow.Features'Last then
            Put (", ");
         end if;
      end loop;
      Put_Line ("), Label: " & To_String (aRow.Label));

   end Print_Row;

   --  ------------------------------------------------------------------------

   procedure Print_Rows (Message : String; Rows : Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message & ":");
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("  Feature values: (");
         for feat in aRow.Features'First .. aRow.Features'Last loop
            Put (To_String (aRow.Features (feat)));
            if feat /= aRow.Features'Last then
               Put (", ");
            end if;
         end loop;
         Put_Line ("), Label: " & To_String (aRow.Label));
      end loop;

   end Print_Rows;

   --  ------------------------------------------------------------------------

   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Name_Type) is
      use Values_Package;
      Values : constant Value_List := Unique_Values (Rows, Feature);
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

   function Split_String (aString, Pattern : String) return String_List is
      use Ada.Strings;
      Last       : constant Integer := aString'Last;
      Last_Char  : constant Character := aString (Last);
      UB_String  : Unbounded_String;
      Split_List : String_List;
   begin
      if Character'Pos (Last_Char) < 32 then
         UB_String :=
           To_Unbounded_String (aString (aString'First .. Last - 1));
      else
         UB_String := To_Unbounded_String (aString);
      end if;

      declare
         String_2 : constant String := To_String (UB_String);
         Last_2   : constant Integer := String_2'Last;
         A_Index  : Integer;
         B_Index  : Integer := String_2'First;
      begin
         for index in String_2'First .. Fixed.Count (String_2, Pattern) loop
            A_Index :=
              Fixed.Index (String_2 (B_Index .. Last_2), Pattern);
            --  process string slice in any way
            Split_List.Append
              (To_Unbounded_String (String_2 (B_Index .. A_Index - 1)));
            B_Index := A_Index + Pattern'Length;
         end loop;
         --  process last string
         Split_List.Append
           (To_Unbounded_String (String_2 (B_Index .. Last_2)));
      end;
      return Split_List;

   end Split_String;

   --  -------------------------------------------------------------------------

   function Split_String_On_Spaces (aString : String) return String_List is
      use Ada.Strings;
      Routine_Name : constant String := "Utilities.Split_String_On_Spaces ";
      HT_String    : constant String (1 .. 1) :=
                       (1 => Ada.Characters.Latin_1.HT);
      Last_Char    : constant Integer := aString'Last;
      A_Index      : Integer := 1;
      B_Index      : Integer := aString'First;
      Split_List   : String_List;
   begin
      while B_Index < aString'Last and A_Index > 0 loop
         A_Index :=
           Fixed.Index (aString (B_Index .. Last_Char), " ");
         if A_Index = 0 then
            A_Index :=
              Fixed.Index (aString (B_Index .. Last_Char),
                           HT_String);
         end if;

         Put_Line (Routine_Name & "A and B indices:" & Integer'Image (A_Index)
                   & "," & Integer'Image (B_Index));
         Split_List.Append
           (To_Unbounded_String (aString (B_Index .. A_Index - 1)));
         B_Index := A_Index + 1;
         while aString (B_Index) = ' ' loop
            B_Index := B_Index + 1;
         end loop;
      end loop;

      --  process last string
      if B_Index < Last_Char then
         Split_List.Append
           (To_Unbounded_String (aString (B_Index .. Last_Char)));
      end if;

      return Split_List;

   end Split_String_On_Spaces;

   --  -------------------------------------------------------------------------
   --  Swap swaps matrix rows
   procedure Swap (Data : in out Binary_Matrix; L, R : Positive) is
      Val : Natural;
   begin
      for col in Data'First (2) .. Data'Last (2) loop
         Val := Data (L, col);
         Data (L, col) := Data (R, col);
         Data (R, col) := Val;
      end loop;

   end Swap;

   --  -------------------------------------------------------------------------
   --  Swap swaps matrix rows
   procedure Swap (Data : in out Boolean_Matrix; L, R : Positive) is
      Val : Boolean;
   begin
      for col in Data'First (2) .. Data'Last (2) loop
         Val := Data (L, col);
         Data (L, col) := Data (R, col);
         Data (R, col) := Val;
      end loop;

   end Swap;

   --  -------------------------------------------------------------------------
   --  Swap swaps matrix rows
   procedure Swap (Data : in out  ML_Types.Real_Float_Matrix;
                   L, R : Positive) is
      Val : Float;
   begin
      for col in Data'First (2) .. Data'Last (2) loop
         Val := Data (L, col);
         Data (L, col) := Data (R, col);
         Data (R, col) := Val;
      end loop;

   end Swap;

   --  -------------------------------------------------------------------------

   procedure Swap (Data : in out Float_Array; L, R : Positive) is
      Item : Float;
   begin
      Item := Data (L);
      Data (L) := Data (R);
      Data (R) := Item;

   end Swap;

   --  -------------------------------------------------------------------------

   procedure Swap (Data : in out Integer_Array; L, R : Positive) is
      Item : Integer;
   begin
      Item := Data (L);
      Data (L) := Data (R);
      Data (R) := Item;

   end Swap;

   --  -------------------------------------------------------------------------

   procedure Swap (Data : in out Integer_Matrix; L, R : Positive) is
      Val : Integer;
   begin
      for col in Data'First (2) .. Data'Last (2) loop
         Val := Data (L, col);
         Data (L, col) := Data (R, col);
         Data (R, col) := Val;
      end loop;

   end Swap;

   --  -------------------------------------------------------------------------

   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Name_Type) return Value_List is
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
      theSet            : Value_List;

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

   function XY_To_Rows (X, Y : Value_Data_Lists_2D)
                        return Rows_Vector is

      Feature_Values   : Value_Data_List;
      Label_Values     : Value_Data_List;
      aRow             : Row_Data;
      Rows             : Rows_Vector;
   begin
      for index in 1 .. Positive (X.Length) loop
         Feature_Values := X.Element (index);
         Label_Values := Y.Element (index);
         for index2 in Feature_Values.First_Index ..
           Feature_Values.Last_Index loop
            aRow.Features (Class_Range (index)) :=
              Feature_Values.Element (index2).UB_String_Value;
            aRow.Label :=
              Label_Values.Element (index2).UB_String_Value;
            Rows.Append (aRow);
         end loop;
      end loop;

      return Rows;

   end XY_To_Rows;

   --  --------------------------------------------------------------------------

end Utilities;
