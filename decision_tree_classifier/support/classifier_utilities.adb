
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;

with Encode_Utils;
--  with Printing;
with Utilities;

package body Classifier_Utilities is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);

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
      use ML_Types;
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
            Diff := Abs (R.Element (index) - L.Element (index));
            OK := OK and (Diff < 1.5 * 10.0 ** (-6));
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
      use Tree;
      use Nodes_Package;
      Nodes     : constant Nodes_Package.Tree :=
                    aClassifier.Attributes.Decision_Tree.Nodes;
      Num_Nodes : Natural := 0;
      procedure Add (Curs : Cursor) is
         Node : constant Tree_Node := Element (Curs);
      begin
         if Curs /= Nodes.Root then
            Num_Nodes := Num_Nodes + Node.Num_Node_Samples;
         end if;
      end Add;
   begin
      Iterate (Nodes, Add'Access);
      return Num_Nodes;

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

   function Get_Column
     (List_2D      : ML_Types.Value_Data_Lists_2D;
      Column_Index : Positive) return  ML_Types.Value_Data_List is
      use  ML_Types;
      aList  : Value_Data_List;
      Column : Value_Data_List;
      Data   : Value_Record;
   begin
      for index in List_2D.First_Index .. List_2D.Last_Index loop
         aList := List_2D.Element (index);
         Data := aList.Element (Column_Index);
         Column.Append (aList);
      end loop;

      return Column;

   end Get_Column;

   --  -------------------------------------------------------------------------

   function Init_Samples_Copy (Samples : ML_Types.Value_Data_Lists_2D)
                                return ML_Types.Value_Data_Lists_2D is
      use ML_Types;
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

   function Load_Data (File_Name : String) return ML_Types.Data_Record is
      Data_File : File_Type;
      CSV_Data  : ML_Types.Rows_Vector;
   begin
      Open (Data_File, In_File, File_Name);
      Utilities.Load_CSV_Data (Data_File, CSV_Data);
      Close (Data_File);

      return Utilities.Split_Row_Data (CSV_Data);

   end Load_Data;

   --  -------------------------------------------------------------------------
   --  Nodes_3D_To_Outputs_3D num nodes x num outputs x num classes to
   --                         num outputs x num nodes x num classes
   function Nodes_3D_To_Outputs_3D (Nodes       : Weights.Weight_Lists_3D;
                                    Num_Outputs : Positive)
                                     return Weights.Weight_Lists_3D is
      use Weights;
      Node         : Weight_Lists_2D;
      Classes      : Weight_List;
      Node_Classes : Weight_Lists_2D;  --  num nodes x num classes
      --             Outputs: num_outputs x num nodes x num classes
      Outputs      : Weight_Lists_3D;
   begin
      for index in 1 .. Num_Outputs loop
         Node_Classes.Clear;
         for index_2 in Nodes.First_Index .. Nodes.Last_Index loop
            --  Node: num outputs x num classes
            Node := Nodes.Element (index_2);
            for index_3 in Node.First_Index .. Node.Last_Index loop
               --  Classes: num classes
               Classes := Node.Element (index_3);
            end loop;
            Node_Classes.Append (Classes);
         end loop;
         Outputs.Append (Node_Classes);
      end loop;

      return Outputs;

   end Nodes_3D_To_Outputs_3D;

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
                                       return Integer_List is
      use ML_Types;
      use Integer_Package;
      use Value_Data_Package;
      use Value_Data_Sorting;
      Item    : Value_Record;
      Index_A : Positive;
      theList : Integer_List;
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
      use ML_Types;
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

   function To_Array (L : Integer_List) return Integer_Array is
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

   function To_Integer_List (A : Integer_Array) return Integer_List is
      A_List : Integer_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

   end To_Integer_List;

   --  -------------------------------------------------------------------------

   function To_Integer_Value_List (A : Integer_Array)
                                    return ML_Types.Value_Data_List is
      use ML_Types;
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
      use ML_Types;
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
      use ML_Types;
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

   function To_Value_2D_List (A : ML_Types.Value_Data_List)
                               return ML_Types.Value_Data_Lists_2D is
      use ML_Types;
      use Value_Data_Package;
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

   function To_Natural_List (A : Natural_Array) return Natural_List is
      A_List : Natural_List;
   begin
      for index in A'First .. A'Last loop
         A_List.Append (A (index));
      end loop;
      return A_List;

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

   function Transpose (Values : ML_Types.Value_Data_Lists_2D)
                        return  ML_Types.Value_Data_Lists_2D is
      use  ML_Types;
      New_Row : Value_Data_List;
      Result  : Value_Data_Lists_2D;
   begin
      for col in 1 .. Positive (Values.Element (1).Length) loop
         New_Row := Get_Column (Values, col);
         Result.Append (New_Row);
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

   function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                            return Tree.Tree_Cursor is
      use Ada.Containers;
      use Tree;
      use Nodes_Package;
      Parent_Node : constant Tree_Cursor := Parent (Current_Node);
      Next_Node   : Tree_Cursor;
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
      use Integer_Package;
      Unique_Set : Int_Sets.Set;
      Int_Curs   : Integer_Package.Cursor := Nums.First;
      Set_Curs   : Int_Sets.Cursor;
      Nums_List  : Integer_List;
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

end Classifier_Utilities;
