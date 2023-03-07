
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with Encode_Utils;

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
                     return NL_Types.Natural_List is
      Values       : Weights.Weight_List;
      Max_Indices  : NL_Types.Natural_List;
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
                     return NL_Types.Natural_List is
      Values_2K    : Weights.Weight_Lists_2D;
      Values       : Weights.Weight_List;
      Max_Indices  : NL_Types.Natural_List;
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
   function Bin_Count (Numbers : NL_Types.Natural_List)
                       return NL_Types.Natural_List is
      use Ada.Containers;
      use NL_Types.Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : NL_Types.Natural_List;
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
                       return NL_Types.Natural_List is
      use Ada.Containers;
      use NL_Types.Natural_Package;
      aNumber    : Natural := 0;
      Max_Number : Natural := 0;
      Bins       : NL_Types.Natural_List;
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

   function Compare_Float_Lists (L, R : NL_Types.Float_List) return Boolean is
      use Ada.Containers;
      use NL_Types.Float_Package;
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
                 R : NL_Types.Natural_List) return Float is
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

   function Get_Column (List_2D      : NL_Types.Float_List_2D;
                        Column_Index : Positive) return NL_Types.Float_List is
      aList  : NL_Types.Float_List;
      Column : NL_Types.Float_List;
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

   function Max_Probability_Indices (Probabilities : Binary_Matrix)
                                     return Integer_Array is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Max_Probability_Indices ";
      Max_Values  : Integer_Array (Probabilities'Range) := (others => 0);
      Value       : Integer;
      Max_Indices : Integer_Array (Probabilities'Range) := (others => 1);
   begin
      for row in Probabilities'Range loop
         for col in Probabilities'Range (2) loop
            Value := Probabilities (row, col);
            if Value > Max_Values (row) then
               Max_Indices (row) := col;
               Max_Values (row) := Value;
            end if;
         end loop;

      end loop;

      return Max_Indices;

   end Max_Probability_Indices;

   --  ------------------------------------------------------------------------

   function Max_Probability_Indices (Probabilities : Real_Float_Matrix)
                                     return Integer_Array is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Max_Probability_Indices ";
      Max_Values  : Real_Float_Vector (Probabilities'Range) := (others => 0.0);
      Value       : Float;
      Max_Indices : Integer_Array (Probabilities'Range) := (others => 1);
   begin
      for row in Probabilities'Range loop
         for col in Probabilities'Range (2) loop
            Value := Probabilities (row, col);
            if Value > Max_Values (row) then
               Max_Indices (row) := col;
               Max_Values (row) := Value;
            end if;
         end loop;

      end loop;

      return Max_Indices;

   end Max_Probability_Indices;

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

   function Probabilities (aMatrix : Real_Float_Matrix)
                           return Real_Float_Matrix is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Probabilities Matrix ";
      Sum    : Float;
      Result : Real_Float_Matrix (aMatrix'Range, aMatrix'Range (2));
   begin
      for row in aMatrix'Range loop
         Sum := 0.0;
         for col in aMatrix'Range (2) loop
            Sum := Sum + aMatrix (row, col);
         end loop;

         for col in aMatrix'Range (2) loop
            Result (row, col) := aMatrix (row, col) / Sum;
         end loop;
      end loop;

      return Result;

   end Probabilities;

   --  ------------------------------------------------------------------------

   function Probabilities (Vec : Real_Float_Vector) return Real_Float_Vector is
      --        Routine_Name : constant String :=
      --                           "Classifier_Utilities.Probabilities Vec";
      Sum    : Float := 0.0;
      Result : Real_Float_Vector (Vec'Range);
   begin
      for row in Vec'Range loop
         Sum := Sum + Vec (row);
      end loop;

      for row in Vec'Range loop
         Result (row) := Vec (row) / Sum;
      end loop;

      return Result;

   end Probabilities;

   --  ------------------------------------------------------------------------
   --  Row_Max_Indices returns the indices of the maximum value in each row of
   --  a matrix.
   function Row_Max_Indices (Values : Boolean_Matrix) return Natural_Array is
      Indices     : Natural_Array (1 .. Values'Length);
      Max_Value   : Boolean := False;
      Max_Index   : Positive;
      Col         : Natural;
   begin
      for row in Values'Range loop
         Max_Value := False;
         Max_Index := 1;
         Col := 0;
         while Col < Values'Last (2) and not Max_Value loop
            Col := Col + 1;
            if Values (row, Col) then
               Max_Index := Col;
               Max_Value := Values (row, col);
            end if;
         end loop;
         Indices (row) := Max_Index;
      end loop;

      return Indices;

   end Row_Max_Indices;

   --  -----------------------------------------------------------------------
   --  Row_Max_Indices returns the indices of the maximum value in each row
   --  of a matrix.
   function Row_Max_Indices (Values : Real_Float_Matrix) return Natural_Array is
      Indices   : Natural_Array (1 .. Values'Length);
      Max_Value : Float;
      Max_Index : Positive;
      Col       : Natural;
   begin
      for row in Values'Range loop
         Max_Value := Values (row, 1);
         Max_Index := 1;
         Col := 1;
         while Col < Values'Last (2) loop
            Col := Col + 1;
            if Values (row, Col) > Max_Value then
               Max_Index := Col;
               Max_Value := Values (row, col);
            end if;
         end loop;
         Indices (row) := Max_Index;
      end loop;

      return Indices;

   end Row_Max_Indices;

   --  -----------------------------------------------------------------------
   --  Search_Sorted_Integer_List finds the indices into List_A such that,
   --  if the corresponding elements in List_B were inserted before the indices,
   --  the order of List_A would be preserved.
   --  The Search_Sorted functions returns the indices where new elements in
   --  List_B should be inserted into List_A to keep List_A sorted.
   function Search_Sorted_Integer_List (List_A, List_B : ML_Types.Integer_List)
                                        return ML_Types.Integer_List is
      use ML_Types.Integer_Package;
      use ML_Types.Integer_Sorting;
      Item      : Integer;
      Pos_Found : Boolean;
      theList   : ML_Types.Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
           "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Pos_Found := False;
         for index_A in List_A.First_Index .. List_A.Last_Index loop
            if not Pos_Found then
               Pos_Found := List_A (index_A) > Item;
               if Pos_Found then
                  theList.Append (Index_A);
               end if;
            end if;
         end loop;

         if not Pos_Found then
            theList.Append (List_A.Last_Index + 1);
         end if;
      end loop;

      return theList;

   end Search_Sorted_Integer_List;

   --  -------------------------------------------------------------------------

   function Search_Sorted_Float_List (List_A, List_B : NL_Types.Float_List)
                                      return ML_Types.Integer_List is
      use NL_Types.Float_Package;
      use NL_Types.Float_Sorting;
      Routine_Name : constant String :=
                       "Classifier_Utilities.Search_Sorted_Float_List ";
      Item         : Float;
      Pos_Found    : Boolean;
      theList      : ML_Types.Integer_List;
   begin
      if not Is_Sorted (List_A) then
         raise Value_Error with
         Routine_Name & "Search_Sorted called with unsorted list.";
      end if;

      for index_B in List_B.First_Index .. List_B.Last_Index loop
         Item := List_B.Element (index_B);
         Pos_Found := False;
         for index_A in List_A.First_Index .. List_A.Last_Index loop
            if not Pos_Found then
               Pos_Found := List_A (index_A) > Item;
               if Pos_Found then
                  theList.Append (Index_A);
               end if;
            end if;
         end loop;

         if not Pos_Found then
            theList.Append (List_A.Last_Index + 1);
         end if;
      end loop;

      return theList;

   end Search_Sorted_Float_List;

   --  -------------------------------------------------------------------------

   function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                      return ML_Types.Integer_List is
      use ML_Types.Integer_Package;
      use Value_Data_Sorting;
      Item    : Value_Record;
      Index_A : Positive;
      theList : ML_Types.Integer_List;
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

   function Set_Diff (Values : Integer_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List is
      use NL_Types;
      use NL_Types.Natural_Package;
      Unique_Vals : constant Integer_Array := Encode_Utils.Unique (Values);
      aVal        : Integer;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : NL_Types.Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;

         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;

   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values : Integer_Array; Uniques : Natural_Array)
                      return NL_Types.Natural_List is
      use NL_Types;
      use NL_Types.Natural_Package;
      Unique_Vals : constant Integer_Array := Encode_Utils.Unique (Values);
      aVal        : Natural;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values : Natural_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List is
      use NL_Types;
      use NL_Types.Natural_Package;
      Unique_Vals : constant Natural_Array := Encode_Utils.Unique (Values);
      aVal        : Natural;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Natural_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;
   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values, Uniques : Boolean_Array)
                      return NL_Types.Boolean_List is
      use NL_Types;
      Unique_Vals : constant Boolean_Array := Encode_Utils.Unique (Values);
      aVal        : Boolean;
      U_Index     : Positive;
      Found       : Boolean;
      Diff        : Boolean_List;
   begin
      for index in Unique_Vals'Range loop
         aVal := Unique_Vals (index);
         Found := False;
         U_Index := Uniques'First;
         while U_Index <= Uniques'Last and not Found loop
            Found := Uniques (U_Index) = aVal;
            U_Index := U_Index + 1;
         end loop;
         if Found then
            Diff.Append (aVal);
         end if;
      end loop;

      return Diff;

   end Set_Diff;

   --  -------------------------------------------------------------------------

   function Set_Diff (Values, Uniques : NL_Types.Natural_List)
                      return NL_Types.Natural_List is
      use NL_Types.Natural_Package;
      Unique_Vals : constant NL_Types.Natural_List :=
                      Encode_Utils.Unique (Values);
      aVal        : Natural;
      Diff        : NL_Types.Natural_List;
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

   function Sum_Cols (aList : NL_Types.Float_List_2D)
                      return NL_Types.Float_List is
      theSum : NL_Types.Float_List;
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

   function Unique (Nums : Integer_List) return Integer_List is
      use Int_Sets;
      use ML_Types.Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : ML_Types.Integer_List;
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
      use NL_Types.Float_Package;
      use Weight_Lists_2D_Package;
      Unique_Set     : Weight_Sets.Set;
      Weight_3D_Curs : Weight_Lists_3D_Package.Cursor := Values.First;
      Weight_2D_Curs : Weight_Lists_2D_Package.Cursor;
      Weights_Curs   : NL_Types.Float_Package.Cursor;
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
