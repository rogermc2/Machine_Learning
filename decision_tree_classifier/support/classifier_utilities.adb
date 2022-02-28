
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Encode_Utils;
--  with Printing;
with Utilities;

package body Classifier_Utilities is

   use ML_Types;

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   use ML_Types.Value_Data_Package;
   package Value_Sets is new
     Ada.Containers.Ordered_Sets (ML_Types.Value_Record);
   use Weights.Weight_Lists_3D_Package;
   package Weight_Sets is new
     Ada.Containers.Ordered_Sets (Float);

   package Float_IO is new Ada.Text_IO.Float_IO (Num => Float);

   function Split_Raw_Data (Raw_Data    : ML_Types.Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                            return Multi_Output_Data_Record;

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the indices of the maximum values along an axis.
   function Arg_Max (Values : Weights.Weight_List) return Positive is
      Max_Value  : Float := Float'Safe_First;
      Max_Index  : Positive := 1;
      Value      : Float;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Value := Values.Element (index);
         if Value > Max_Value then
            Max_Index := index;
            Max_Value := Value;
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the indices of the maximum values along an axis.
   --  Numpy argmax(input_array, axis=None, out=None)
   --  axis : int, optional  By default the index is into the flattened array,
   --  otherwise along the specified axis.
   --  Returns an array of indices into the input_array.
   --  It has the same shape as `input_array.shape` with the dimension along
   --  `axis` removed.
   --  Example a = array([[10, 11, 12],   indices: [[00, 01, 02],
   --                     [13, 14, 15]])           [[10, 11, 12],
   --  2 rows (axis 0) x 3 columns  (axis 1)
   --  np.argmax(a, axis=1)
   --  returns array([2, 2]) -> [12, 15]
   function Arg_Max (Values_2D : Weights.Weight_Lists_2D; Axis : Natural := 0)
                     return Classifier_Types.Natural_List is
      Values       : Weights.Weight_List;
      Max_Indices  : Classifier_Types.Natural_List;
   begin
      Max_Indices.Clear;
      if Axis = 0 then
         for index in Values_2D.First_Index .. Values_2D.Last_Index loop
            Values := Values_2D.Element (index);
            Max_Indices.Append (Weights.Max (Values));
         end loop;
      end if;

      return Max_Indices;

   end Arg_Max;

   --  -------------------------------------------------------------------------

   function Arg_Max (Values_3D : Weights.Weight_Lists_3D; Axis : Natural := 0)
                     return Classifier_Types.Natural_List is
      Values_2K    : Weights.Weight_Lists_2D;
      Values       : Weights.Weight_List;
      Max_Indices  : Classifier_Types.Natural_List;
   begin
      Values_2K.Clear;
      Max_Indices.Clear;
      if Axis = 0 then
         for index_1 in Values_3D.First_Index .. Values_3D.Last_Index loop
            Values_2K := Values_3D (index_1);
            for index_2 in Values_2K.First_Index .. Values_2K.Last_Index loop
               Values := Values_2K.Element (index_2);
               Max_Indices.Append (Weights.Max (Values));
            end loop;
         end loop;
      end if;

      return Max_Indices;

   end Arg_Max;

   --  -------------------------------------------------------------------------
   --  Bin_Count counts the number of occurrences of each value in Numbers.
   --  Each bin gives the number of occurrences of its index value in Numbers.
   function Bin_Count (Numbers : Natural_List) return Natural_List is
      use Ada.Containers;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index);
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   function Bin_Count (Numbers : ML_Types.Value_Data_List)
                       return Natural_List is
      use Ada.Containers;
      use Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         if Numbers.Element (index).Value_Kind /= Integer_Type then
            raise Value_Error with
              "Classifier_Utilities.Bin_Count called with non-integer data "
              & Data_Type'Image (Numbers.Element (index).Value_Kind);
         end if;

         aNumber := Numbers.Element (index).Integer_Value;
         if aNumber > Max_Number then
            Max_Number := aNumber;
         end if;
      end loop;

      Bins.Set_Length (Count_Type (Max_Number + 1));
      for index in Bins.First_Index .. Bins.Last_Index loop
         Bins (index) := 0;
      end loop;

      for index in Numbers.First_Index .. Numbers.Last_Index loop
         aNumber := Numbers.Element (index).Integer_Value;
         Bins (aNumber + 1) := Bins (aNumber + 1) + 1;
      end loop;

      return Bins;

   end Bin_Count;

   --  -------------------------------------------------------------------------

   procedure Clear (anArray : in out ML_Types.Value_Data_Array) is
   begin
      for index in anArray'Range loop
         anArray (index).Float_Value := 0.0;
      end loop;
   end Clear;

   --  -------------------------------------------------------------------------

   function Compare_Float_Lists (L, R : Float_List) return Boolean is
      use Ada.Containers;
      use Float_Package;
      Diff     : Float := 0.0;
      Max_Diff : Float := 0.0;
      OK       : Boolean := R.Length = L.Length;
   begin
      if OK then
         for index in L.First_Index .. L.Last_Index loop
            Diff := abs (R.Element (index) - L.Element (index));
            OK := OK and then (Diff < 1.5 * 10.0 ** (-6));
            if Diff > Max_Diff then
               Max_Diff := Diff;
            end if;
         end loop;

         if not OK then
            New_Line;
            Put ("Classifier_Utilities.Compare_Float_Lists test ");
            Put_Line ("failed with error: " & Float'Image (Max_Diff));
            New_Line;
         end if;
      else
         New_Line;
         Put ("Classifier_Utilities.Compare_Float_Lists ");
         Put_Line ("test failed with different length lists, Left: "
                   & Count_Type'Image (L.Length) & ", Right: " &
                     Count_Type'Image (R.Length));
         New_Line;
      end if;

      return OK;
   end Compare_Float_Lists;

   --  -------------------------------------------------------------------------

   function Count_Samples (aClassifier : Base_Decision_Tree.Classifier)
                           return Natural is
      use Ada.Containers;
      use Tree;
      use Nodes_Package;
      Nodes       : constant Nodes_Package.Tree :=
                      aClassifier.Attributes.Decision_Tree.Nodes;
      Num_Samples : Natural := 0;

      procedure Add (Curs : Nodes_Package.Cursor) is
         Node : constant Tree_Node := Element (Curs);
      begin
         if Curs /= Nodes.Root then
            Num_Samples := Num_Samples + Node.Num_Node_Samples;
         end if;
      end Add;

   begin
      Iterate (Nodes, Add'Access);
      return Num_Samples / Integer (Nodes.Node_Count - 2);

   end Count_Samples;

   --  -------------------------------------------------------------------------

   function Dot (L : Weights.Weight_List;
                 R : Natural_List) return Float is
      use Float_Package;
      Result : Float := 0.0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * Float (R.Element (index));
      end loop;

      return Result;

   end Dot;

   --  -------------------------------------------------------------------------

   function Float_Precision (Number : Float; Precision : Natural)
                             return String is
      use Ada.Numerics.Elementary_Functions;
      Integer_Length : Positive;
      String_Length  : Positive;
   begin
      if abs Number < 1.0 then
         Integer_Length := 1;
      else
         Integer_Length :=
           1 + Integer (Float'Floor (Log (Float'Floor (abs Number), 10.0)));
      end if;

      String_Length := Integer_Length + Precision + 1;
      if Number < 0.0 then
         String_Length := String_Length + 1;
      end if;

      declare
         theString : String (1 .. String_Length);
      begin
         Float_IO.Put (To   => theString,
                       Item => Number,
                       Aft  => Precision,
                       Exp  => 0);
         return theString;
      end;

   end Float_Precision;

   --  -------------------------------------------------------------------------

   function Get_Column (List_2D      : Float_List_2D;
                        Column_Index : Positive) return Float_List is
      aList  : Float_List;
      Column : Float_List;
      Data   : Float;
   begin
      for index in List_2D.First_Index .. List_2D.Last_Index loop
         aList := List_2D.Element (index);
         Data := aList.Element (Column_Index);
         Column.Append (Data);
      end loop;

      return Column;

   end Get_Column;

   --  -------------------------------------------------------------------------

   function Init_Samples_Copy (Samples : ML_Types.Value_Data_Lists_2D)
                               return ML_Types.Value_Data_Lists_2D is
      Num_Samples    : constant Positive := Positive (Samples.Length);
      Sample_1       : constant Value_Data_List := Samples.Element (1);
      Num_Features   : constant Positive := Positive (Sample_1.Length);
      Boolean_Init   : Value_Record (Boolean_Type);
      Float_Init     : Value_Record (Float_Type);
      Integer_Init   : Value_Record (Integer_Type);
      UB_String_Init : Value_Record (UB_String_Type);
      Init_Features  : Value_Data_List;
      theCopy        : Value_Data_Lists_2D;
   begin
      for index in 1 .. Num_Features loop
         case Sample_1.Element (index).Value_Kind is
            when Boolean_Type =>
               Init_Features.Append (Boolean_Init);
            when Float_Type =>
               Init_Features.Append (Float_Init);
            when Integer_Type =>
               Init_Features.Append (Integer_Init);
            when UB_String_Type =>
               Init_Features.Append (UB_String_Init);
         end case;
      end loop;

      for index in 1 .. Num_Samples loop
         theCopy.Append (Init_Features);
      end loop;

      return theCopy;

   end Init_Samples_Copy;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String; Num_Outputs : Positive := 1)
                       return ML_Types.Multi_Output_Data_Record is
      Data_File    : File_Type;
      Raw_CSV_Data : ML_Types.Raw_Data_Vector;
      Output_Data  : ML_Types.Multi_Output_Data_Record;
   begin
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Utilities.Load_Raw_CSV_Data (Data_File);
      Close (Data_File);

      Output_Data := Split_Raw_Data (Raw_CSV_Data, Num_Outputs);
      return Output_Data;

   end Load_Data;

   --  ---------------------------------------------------------------------------

   procedure Parse_Header
     (Header       : ML_Types.Unbounded_List; Num_Features : Positive;
      Data_Record  : in out Multi_Output_Data_Record) is
   begin
      for index in 1 .. Positive (Header.Length) loop
         if index <= Num_Features then
            Data_Record.Feature_Names.Append (Header.Element (index));
         else
            Data_Record.Label_Names.Append (Header.Element (index));
         end if;
      end loop;

   end Parse_Header;

   --  -------------------------------------------------------------------------
   --  Samples_3D_To_Outputs_3D num samples x num outputs x num classes to
   --                           num outputs x num samples x num classes
   function Samples_3D_To_Outputs_3D (Samples     : Weights.Weight_Lists_3D;
                                      Num_Outputs : Positive)
                                      return Weights.Weight_Lists_3D is
      use Weights;
      Sample_List    : Weight_Lists_2D;
      Classes        : Weight_List;
      Sample_Classes : Weight_Lists_2D;  --  num samples x num classes
      --  Outputs: num_outputs x num samples x num classes
      Outputs        : Weight_Lists_3D;
   begin
      for index in 1 .. Num_Outputs loop
         Sample_Classes.Clear;
         for index_2 in Samples.First_Index .. Samples.Last_Index loop
            --  Sample_List: num outputs x num classes
            Sample_List := Samples.Element (index_2);
            for index_3 in Sample_List.First_Index
              .. Sample_List.Last_Index loop
               --  Classes: num classes
               Classes := Sample_List.Element (index_3);
            end loop;
            Sample_Classes.Append (Classes);
         end loop;
         Outputs.Append (Sample_Classes);
      end loop;

      return Outputs;

   end Samples_3D_To_Outputs_3D;

   --  -------------------------------------------------------------------------

   function Ones (List_Length : Positive) return Weights.Weight_List is
      List_Of_Ones : Weights.Weight_List;
   begin
      for index in 1 .. List_Length loop
         List_Of_Ones.Append (1.0);
      end loop;

      return List_Of_Ones;

   end Ones;

   --  -------------------------------------------------------------------------

   function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                      return Classifier_Types.Integer_List is
      use Classifier_Types.Integer_Package;
      use Value_Data_Sorting;
      Item    : Value_Record;
      Index_A : Positive;
      theList : Classifier_Types.Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
           "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Index_A := List_A.Find_Index (Item);
         if Index_A /= Value_Data_Package.No_Index then
            theList.Append (Index_A);
         end if;
      end loop;
      return theList;
   end Search_Sorted_Value_List;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values, Uniques : Natural_List) return Natural_List is
      use Natural_Package;
      Unique_Vals : constant Natural_List := Encode_Utils.Unique (Values);
      aVal        : Natural;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals.First_Index .. Unique_Vals.Last_Index loop
         aVal := Unique_Vals.Element (index);
         if not Uniques.Contains (aVal) then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Value (List_Length : Positive; Value : Float)
                       return Weights.Weight_List is
      List_Of_Ones : Weights.Weight_List;
   begin
      for index in 1 .. List_Length loop
         List_Of_Ones.Append (Value);
      end loop;

      return List_Of_Ones;

   end Set_Value;

   --  -------------------------------------------------------------------------

   function Split_Raw_Data (Raw_Data    : ML_Types.Raw_Data_Vector;
                            Num_Outputs : Positive := 1)
                            return Multi_Output_Data_Record is
      use Ada.Containers;
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      --        Routine_Name   : constant String :=
      --                           "Classifier_Utilities.Split_Raw_Data ";
      aRow           : ML_Types.Unbounded_List := Raw_Data.First_Element;
      Num_Items      : constant Positive := Positive (aRow.Length);
      Num_Features   : constant Positive := Num_Items - Num_Outputs;
      Feature_Types  : Feature_Type_Array (1 .. Num_Features);
      Features_List  : Value_Data_Lists_2D;
      Label_Types    : Label_Type_Array  (1 .. Num_Outputs);
      Label_Values   : Value_Data_List;
      Labels_List    : Value_Data_Lists_2D;
      Feature_Values : Value_Data_List;
      Data           : Multi_Output_Data_Record;
   begin
      Parse_Header (aRow, Num_Features, Data);
      aRow := Raw_Data.Element (Positive'Succ (Raw_Data.First_Index));
      if aRow.Length > 1 then
         for f_index in 1 .. Num_Features loop
            declare
               Row_S     : constant String := To_String (aRow (f_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (f_index) := To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Feature_Types (Positive (f_index)) :=
                 Utilities.Get_Data_Type (aRow (Positive (f_index)));
            end;
         end loop;

         for l_index in 1 .. Num_Outputs loop
            declare
               Row_S     : constant String := To_String (aRow (Num_Features + l_index));
               S_Last    : constant Integer := Row_S'Last;
               Last_Char : constant Character := Row_S (S_Last);
            begin
               if Character'Pos (Last_Char) < 32 then
                  aRow (Num_Features + l_index) := To_Unbounded_String (Row_S (1 .. S_Last - 1));
               end if;
               Label_Types (Positive (l_index)) :=
                 Utilities.Get_Data_Type (aRow (Positive (Num_Features + l_index)));
            end;
         end loop;

         for row_index in Positive'Succ (Raw_Data.First_Index) ..
           Raw_Data.Last_Index loop
            aRow := Raw_Data.Element (row_index);  --  Unbound list

            Feature_Values.Clear;
            for f_index in 1 .. Num_Features loop
               declare
                  Feat_String : constant String := To_String (aRow (f_index));
                  Value       : Value_Record (Feature_Types (Positive (f_index)));
               begin
                  case Feature_Types (Positive (f_index)) is
                  when Boolean_Type =>
                     Value.Boolean_Value := Boolean'Value (Feat_String);
                  when Integer_Type =>
                     Value.Integer_Value := Integer'Value (Feat_String);
                  when Float_Type =>
                     Value.Float_Value := Float'Value (Feat_String);
                  when UB_String_Type =>
                     Value.UB_String_Value := aRow (f_index);
                  end case;
                  Feature_Values.Append (Value);
               end;  --  declare block
            end loop;
            Features_List.Append (Feature_Values);

            for o_index in 1 .. Num_Outputs loop
               Label_Values.Clear;
               declare
                  Row_S     : constant String :=
                                To_String (aRow (Num_Features + o_index));
                  S_Last    : constant Integer := Row_S'Last;
                  Last_Char : constant Character := Row_S (S_Last);
               begin
                  if Character'Pos (Last_Char) < 32 then
                     aRow (Num_Features + o_index) :=
                       To_Unbounded_String (Row_S (1 .. S_Last - 1));
                  end if;
               end;

               declare
                  Label       : constant String :=
                                  To_String (aRow (Num_Features + o_index));
                  Label_Value : Value_Record (Label_Types (o_index));
               begin
                  case Label_Types (Positive (o_index)) is
                  when Boolean_Type =>
                     Label_Value.Boolean_Value := Boolean'Value (Label);
                  when Integer_Type =>
                     Label_Value.Integer_Value := Integer'Value (Label);
                  when Float_Type =>
                     Label_Value.Float_Value := Float'Value (Label);
                  when UB_String_Type =>
                     Label_Value.UB_String_Value :=
                       aRow (Num_Features + o_index);
                  end case;
                  Label_Values.Append (Label_Value);
               end;  --  declare block;
               Labels_List.Append (Label_Values);
            end loop;
         end loop;

         Data.Feature_Values := Features_List;
         Data.Label_Values := Labels_List;
      end if;

      return Data;

   end Split_Raw_Data;

   --  -----------------------------------------------------------------------

   function Sum_Cols (aList : Classifier_Types.Float_List_2D)
                      return Classifier_Types.Float_List is
      theSum : Classifier_Types.Float_List;
      Value  : Float;
   begin
      for index in aList.First_Index .. aList.Last_Index loop
         Value := 0.0;
         for index_2 in aList.Element (1).First_Index ..
           aList.Element (1).Last_Index loop
            Value := Value + aList.Element (1).Element (index_2);
         end loop;
         theSum.Append (Value);
      end loop;

      return theSum;

   end Sum_Cols;

   --  -------------------------------------------------------------------------

   function Sum_Cols (aList : ML_Types.Value_Data_Lists_2D)
                      return ML_Types.Value_Data_List is
      theSum     : Value_Data_List;
      Value_Type : constant Data_Type :=
                     aList.Element (1).Element (1).Value_Kind;
      F_Value    : Float;
      I_Value    : Integer;
      Value_Rec  : Value_Record;
   begin
      for index in aList.First_Index .. aList.Last_Index loop
         F_Value := 0.0;
         I_Value := 0;
         Value_Rec := aList.Element (1).Element (1);
         case Value_Type is
            when Float_Type =>
               for index_2 in aList.Element (1).First_Index ..
                 aList.Element (1).Last_Index loop
                  F_Value := F_Value +
                    aList.Element (1).Element (index_2).Float_Value;
               end loop;
               Value_Rec.Float_Value := F_Value;

            when Integer_Type =>
               for index_2 in aList.Element (1).First_Index ..
                 aList.Element (1).Last_Index loop
                  I_Value := I_Value +
                    aList.Element (1).Element (index_2).Integer_Value;
               end loop;
               Value_Rec.Integer_Value := I_Value;

            when others => null;
         end case;
         theSum.Append (Value_Rec);
      end loop;

      return theSum;

   end Sum_Cols;

   --  -------------------------------------------------------------------------
   --  aList: num outputs x num samples x num classes
   --  Sum_Cols sums each class
   function Sum_Cols (aList : Weights.Weight_Lists_3D)
                      return Weights.Weight_List is
      Samples : Weights.Weight_Lists_2D;
      Classes : Weights.Weight_List;
      Sums    : Weights.Weight_List;
      Value   : Float := 0.0;
   begin
      for index in 1 .. aList.Element (1).Length loop
         Sums.Append (0.0);
      end loop;

      for output_index in aList.First_Index .. aList.Last_Index loop
         Samples := aList.Element (output_index);
         for sample_index in Samples.First_Index .. Samples.Last_Index loop
            Value := 0.0;
            Classes := Samples.Element (sample_index);
            for class_index in Classes.First_Index ..
              Classes.Last_Index loop
               Value := Value + Classes.Element (class_index);
               if abs (Value) < 10.0 ** (-6) then
                  Value := 0.0;
               end if;
            end loop;
            Sums.Replace_Element
              (sample_index, Sums.Element (sample_index) + Value);
         end loop;
      end loop;

      return Sums;

   end Sum_Cols;

   --  -------------------------------------------------------------------------

   function To_Array (L : Classifier_Types.Integer_List) return Integer_Array is
      New_Array : Integer_Array (1 .. Integer (L.Length));
      A_Index   : Integer := 0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         A_Index := A_Index + 1;
         New_Array (A_Index) := L.Element (index);
      end loop;
      return New_Array;
   end To_Array;

   --  -------------------------------------------------------------------------

   function To_Float_List (A : Float_Array) return Float_List is
      A_List : Float_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;
   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Float_List (F : ML_Types.Value_Data_List)
                           return Float_List is
      Item   : Value_Record;
      Floats : Float_List;
   begin
      for index in F.First_Index .. F.Last_Index loop
         Item := F.Element (index);
         Assert (Item.Value_Kind = Float_Type,
                 "Classifier_Utilities.To_Float_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Floats.Append (Item.Float_Value);
      end loop;

      return Floats;

   end To_Float_List;

   --  -------------------------------------------------------------------------

   function To_Integer_List (A : Classifier_Types.Integer_Array)
                             return Classifier_Types.Integer_List is
      A_List : Classifier_Types.Integer_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_List (Ints : ML_Types.Value_Data_List)
                             return Classifier_Types.Integer_List is
      Item   : Value_Record;
      Values : Classifier_Types.Integer_List;
   begin
      for index in Ints.First_Index .. Ints.Last_Index loop
         Item := Ints.Element (index);
         Assert (Item.Value_Kind = Integer_Type,
                 "Classifier_Utilities.To_Float_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Values.Append (Item.Integer_Value);
      end loop;

      return Values;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List (A : Integer_Array)
                                   return ML_Types.Value_Data_List is
      Data       : Value_Record (Integer_Type);
      A_List     : Value_Data_List;
   begin
      for index in A'First .. A'Last loop
         Data.Integer_Value := A (index);
         A_List.Append (Data);
      end loop;

      return A_List;
   end To_Integer_Value_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List_2D (A : Integer_Array)
                                      return ML_Types.Value_Data_Lists_2D is
      Data       : Value_Record (Integer_Type);
      B_List     : Value_Data_List;
      Multi_List : Value_Data_Lists_2D;
   begin
      for index in A'First .. A'Last loop
         B_List.Clear;
         Data.Integer_Value := A (index);
         B_List.Append (Data);
         Multi_List.Append (B_List);
      end loop;

      return Multi_List;
   end To_Integer_Value_List_2D;

   --  -------------------------------------------------------------------------

   function To_Multi_Value_List (A : Multi_Value_Array)
                                 return ML_Types.Value_Data_Lists_2D is
      Value    : Value_Record (Integer_Type);
      Row_List : Value_Data_Lists_2D;
      Col_List : Value_Data_List;
   begin
      for row in A'First .. A'Last loop
         Col_List.Clear;
         for col in A'Range (2) loop
            Value.Integer_Value := A (row, col);
            Col_List.Append (Value);
         end loop;
         Row_List.Append (Col_List);
      end loop;
      return Row_List;

   end To_Multi_Value_List;

   --  -------------------------------------------------------------------------

   function To_Natural_List (A : Natural_Array) return Natural_List is
      A_List : Natural_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_List (Numbers : ML_Types.Value_Data_List)
                             return Natural_List is
      Item   : Value_Record;
      Values : Natural_List;
   begin
      for index in Numbers.First_Index .. Numbers.Last_Index loop
         Item := Numbers.Element (index);
         Assert (Item.Value_Kind = Integer_Type,
                 "Classifier_Utilities.To_Natural_List invalid item "
                 & Integer'Image (index) & " data type is " &
                   Data_Type'Image (Item.Value_Kind));
         Assert (Item.Integer_Value >= 0,
                 "Classifier_Utilities.To_Natural_List invalid value "
                 & Integer'Image (Item.Integer_Value) & " should be >= 0.");
         Values.Append (Natural (Item.Integer_Value));
      end loop;

      return Values;

   end To_Natural_List;

   --  -------------------------------------------------------------------------

   function To_Natural_Value_List (A : Natural_Array)
                                   return ML_Types.Value_Data_Lists_2D is
      Int_Array : Integer_Array (1 .. A'Length);
   begin
      for index in A'First .. A'Last loop
         Int_Array (index) := A (index);
      end loop;
      return To_Integer_Value_List_2D (Int_Array);
   end To_Natural_Value_List;

   --  ------------------------------------------------------------------------

   function To_PL_Array (List_1D : AR_Types.AR_Data_List; Num_Rows : Positive)
                         return PLplot_Auxiliary.Real_Matrix is
      use PLplot_Auxiliary;
      Routine_Name : constant String :=
                       "Classifier_Utilities.To_PL_Array ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Result       : Real_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for row in reverse 1 .. Num_Rows loop
         for col in Start .. Start + End_Offset loop
            Result (col - Start + 1, row) :=
              Long_Float (List_1D.Element (col).Float_Value);
         end loop;
         Start := Start + Num_Cols;
      end loop;

      return Result;

   end To_PL_Array;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (A : ML_Types.Value_Data_List)
                              return ML_Types.Value_Data_Lists_2D is
      Output_List : Value_Data_List;
      A2_List     : Value_Data_Lists_2D;
   begin
      A2_List.Clear;
      for index in A.First_Index .. A.Last_Index loop
         Output_List.Clear;
         Output_List.Append (A.Element (index));
         A2_List.Append (Output_List);
      end loop;

      return A2_List;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function To_Value_2D_List (List_1D  : ML_Types.Value_Data_List;
                              Num_Rows : Positive)
                              return ML_Types.Value_Data_Lists_2D is
      Routine_Name : constant String :=
                       "Classifier_Utilities.To_Value_2D_List ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Column_List  : Value_Data_List;
      List_2D      : Value_Data_Lists_2D;
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for index in 1 .. Num_Rows loop
         Column_List.Clear;
         for col in Start .. Start + End_Offset loop
            Column_List.Append (List_1D (col));
         end loop;
         List_2D.Append (Column_List);
         Start := Start + Num_Cols;
      end loop;

      return List_2D;

   end To_Value_2D_List;

   --  -------------------------------------------------------------------------

   function Transpose (Values : ML_Types.Value_Data_Lists_2D)
                       return  ML_Types.Value_Data_Lists_2D is
      use Ada.Containers;
      Num_Rows : constant Positive := Positive (Values.Length);
      Num_Cols : constant Count_Type := Values.Element (1).Length;
      In_Row   : Value_Data_List;
      Out_Row  : Value_Data_List;
      Result   : Value_Data_Lists_2D;
   begin
      --          Put_Line ("Classifier_Utilities.Transpose Num_Rows, Num_Cols: " &
      --                      Positive'Image (Num_Rows) & " x " &
      --                      Count_Type'Image (Num_Cols));
      Result.Set_Length (Num_Cols);
      for row in 1 .. Num_Rows loop
         In_Row := Values.Element (row);
         for index in In_Row.First_Index ..  In_Row.Last_Index loop
            Out_Row := Result.Element (index);
            Out_Row.Append (In_Row.Element (index));
            Result.Replace_Element (index, Out_Row);
         end loop;
      end loop;
      --          Put_Line ("Classifier_Utilities.Transpose result Num_Rows, Num_Cols: " &
      --                      Count_Type'Image (Result.Length) & " x " &
      --                      Count_Type'Image (Result.Element (1).Length));
      --          New_Line;
      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

   function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                           return Tree.Tree_Cursor is
      use Ada.Containers;
      use Tree.Nodes_Package;
      Parent_Node : constant Tree.Tree_Cursor := Parent (Current_Node);
      Next_Node   : Tree.Tree_Cursor;
   begin
      if not Is_Leaf (Current_Node) then
         if Current_Node = First_Child (Parent_Node) then
            if Child_Count (Parent_Node) > 1 then
               Next_Node := Next_Sibling (First_Child (Current_Node));
            end if;
         end if;
      end if;

      return Next_Node;

   end Traverse_Tree;

   --  -------------------------------------------------------------------------

   function Unique (Nums : Classifier_Types.Integer_List)
                    return Classifier_Types.Integer_List is
      use Int_Sets;
      use Classifier_Types.Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Classifier_Types.Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Classifier_Types.Integer_List;
   begin
      while Has_Element (Int_Curs) loop
         Unique_Set.Include (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Nums_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Nums_List;
   end Unique;

   --  -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                  return Integer_Array is
      use Int_Sets;
      Unique_Set : Int_Sets.Set;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index).Integer_Value);
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end; --  declare block
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array is
      use Int_Sets;
      Unique_Set : Int_Sets.Set;
      Set_Curs   : Int_Sets.Cursor;
   begin
      for index in Nums'Range loop
         Unique_Set.Include (Nums (index));
      end loop;

      declare
         Unique_Array : Integer_Array (1 .. Integer (Unique_Set.Length));
         Unique_Index : Integer := 0;
      begin
         Set_Curs := Unique_Set.First;
         while Has_Element (Set_Curs) loop
            Unique_Index := Unique_Index + 1;
            Unique_Array (Unique_Index) := Element (Set_Curs);
            Next (Set_Curs);
         end loop;
         return Unique_Array;
      end;
   end Unique_Integer_Array;

   --  -------------------------------------------------------------------------

   function Unique_Values (Values : ML_Types.Value_Data_List)
                           return ML_Types.Value_Data_List is
      use Value_Sets;
      Unique_Set  : Value_Sets.Set;
      Int_Curs    : Value_Data_Package.Cursor := Values.First;
      Set_Curs    : Value_Sets.Cursor;
      Values_List : Value_Data_List;
   begin
      while Has_Element (Int_Curs) loop
         Unique_Set.Include (Element (Int_Curs));
         Next (Int_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Values_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Values_List;

   end Unique_Values;

   --  -------------------------------------------------------------------------

   function Unique_Weights (Values : Weights.Weight_Lists_3D)
                            return Weights.Weight_List is
      use Weight_Sets;
      use Weights;
      use Float_Package;
      use Weight_Lists_2D_Package;
      Unique_Set     : Weight_Sets.Set;
      Weight_3D_Curs : Weight_Lists_3D_Package.Cursor := Values.First;
      Weight_2D_Curs : Weight_Lists_2D_Package.Cursor;
      Weights_Curs   : Float_Package.Cursor;
      Set_Curs       : Weight_Sets.Cursor;
      Weight_2D_List : Weight_Lists_2D;
      Weights        : Weight_List;
      Unique_List    : Weight_List;
   begin
      while Has_Element (Weight_3D_Curs) loop
         Weight_2D_List := Element (Weight_3D_Curs);
         Weight_2D_Curs := Weight_2D_List.First;
         while Has_Element (Weight_2D_Curs) loop
            Weights := Element (Weight_2D_Curs);
            Weights_Curs := Weights.First;
            while Has_Element (Weights_Curs) loop
               Unique_Set.Include (Element (Weights_Curs));
               Next (Weights_Curs);
            end loop;
            Next (Weight_2D_Curs);
         end loop;
         Next (Weight_3D_Curs);
      end loop;

      Set_Curs := Unique_Set.First;
      while Has_Element (Set_Curs) loop
         Unique_List.Append (Element (Set_Curs));
         Next (Set_Curs);
      end loop;
      return Unique_List;

   end Unique_Weights;

   --  -------------------------------------------------------------------------

end Classifier_Utilities;
