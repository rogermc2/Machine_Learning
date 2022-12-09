
with Ada.Text_IO; use Ada.Text_IO;

package body Neural_Printing is

   package Real_IO is new Float_IO (Float);

   --  ------------------------------------------------------------------------

   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix;
                                  Start : Integer := 1; Finish : Integer := 0)
   is
      Last : Integer;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last then
         for row in Start .. Last loop
            for col in aMatrix'Range (2) loop
               Put (Binary'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Binary_Matrix called with invalid start or finish index.");
      end if;

   end Print_Binary_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Boolean_Array
     (Name : String; anArray : NL_Arrays_And_Matrices.Boolean_Array) is
      Count : Natural := 0;
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put (Boolean'Image (anArray (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 0;
         end if;
      end loop;
      New_Line;

   end Print_Boolean_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Boolean_Matrix (Name  : String; aMatrix : Boolean_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0)
   is
      Last  : Integer;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last then
         for row in Start .. Last loop
            for col in aMatrix'Range (2) loop
               Put (Boolean'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Boolean_Matrix called with invalid start or finish index.");
      end if;

   end Print_Boolean_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                Start : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
      Count : Integer := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Float'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Float_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_List (Name : String; theList : NL_Types.Float_List) is
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ": ");
      end if;

      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (Index) & ": " &
                Float'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 4 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Float_List;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Lists_2D (Name  : String;
                                   Data  : NL_Types.Float_List_2D;
                                   Start : Positive := 1;
                                   Last  : Positive := 10) is
      Stop  : Integer := Last;
   begin
      if Stop > Integer (Data.Length) then
         Stop := Integer (Data.Length);
      end if;
      Put_Line (Name & ": ");
      for index in Start .. Stop loop
         Print_Float_List ("List" & Integer'Image (index),
                           Data.Element (index));
      end loop;
      New_Line;

   end Print_Float_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Lists_3D (Name : String;
                                   Data : NL_Types.Float_List_3D) is
   begin
      Put_Line (Name & ": ");
      for index in Data.First_Index .. Data.Last_Index loop
         Print_Float_Lists_2D ("2D list:", Data.Element (index));
      end loop;
      New_Line;

   end Print_Float_Lists_3D;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix) is
   begin
      Put (Name & ": ");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix) is
   begin
      Put (Name & " size:");
      Put_Line (Integer'Image (aMatrix'Length) & "  x" &
                  Integer'Image (aMatrix'Length (2)));

   end Print_Matrix_Dimensions;

   --  ------------------------------------------------------------------------

   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array) is
   begin
      Put (Name);
      if anArray'Length > 0 and anArray'First > 0 then
         Put_Line (": ");
         for Index in anArray'First .. anArray'Last loop
            Put_Line (Integer'Image (anArray (Index, 1)) & ",  " &
                        Integer'Image (anArray (Index, 2)));
         end loop;
      elsif anArray'Length = 0 then
         Put_Line (" is empty.");
      else
         raise Print_Error with
           "Print_Multi_Value_Array called with invalid index: " &
           Integer'Image (Integer (anArray'First));
      end if;
      New_Line;

   end Print_Multi_Value_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : NL_Types.Natural_Lists_2D) is
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         Print_Natural_List ("", Data.Element (Index));
      end loop;
      New_Line;

   end Print_Natural_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Natural_List (Name    : String;
                                 theList : NL_Types.Natural_List) is
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ": ");
      end if;

      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Natural'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Natural_List;

   --  ------------------------------------------------------------------------

   procedure Print_Real_Vector_List (Name        : String;
                                     Vector_List : Real_Vector_List) is
   begin
      Put_Line (Name & ": ");
      for index in Vector_List.First_Index .. Vector_List.Last_Index loop
         Print_Float_Array ("Item" & Integer'Image (index),
                            Vector_List (Index));
      end loop;

   end Print_Real_Vector_List;

   --  ------------------------------------------------------------------------

   procedure Print_Slice (Name : String; theSlice : NL_Types.Slice_Record) is
   begin
      Put_Line (Name & ": (" &Integer'Image (theSlice.First) & " ," &
                  Integer'Image (theSlice.Last) & ")");

   end Print_Slice;

   --  ------------------------------------------------------------------------

   procedure Print_Slices (Name  : String; theList : NL_Types.Slices_List;
                           Start : Positive := 1; Last : Positive := 10) is
      Slice : NL_Types.Slice_Record;
      Count : Integer := 1;
      Stop  : Integer := Last;
   begin
      if Stop > Integer (theList.Length) then
         Stop := Integer (theList.Length);
      end if;
      Put_Line (Name & ": ");
      for Index in Start .. Stop loop
         Slice := theList.Element (Index);
         Put ("(" &Integer'Image (Slice.First) & " ," &
                Integer'Image (Slice.Last) & ")");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Slices;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Matrix
     (Name      : String; aMatrix : Real_Float_Matrix;
      Start     : Integer := 1; Finish : Integer := 0;
      Col_Start : Integer := 1; Col_Finish : Integer := 0) is
      Last_Row : Integer;
      Last_Col : Integer;
   begin
      if Finish > 0 then
         Last_Row := Finish;
      else
         Last_Row := Integer (aMatrix'Length);
      end if;

      if Col_Finish > 0 then
         Last_Col := Col_Finish;
      else
         Last_Col := Integer (aMatrix'Length (2));
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last and then
        Col_Start >= aMatrix'First (2) and then
        Col_Finish <= aMatrix'Last (2) then
         for row in Start .. Last_Row loop
            Put ("Row" & Integer'Image (row) & ":");
            for col in Col_Start .. Last_Col loop
               Put (Float'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Float_Matrix called with invalid start or finish index.");
      end if;

   end Print_Float_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Matrix_Formated
     (Name  : String; aMatrix : Real_Float_Matrix; Places : Natural;
      Start : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last then
         for row in Start .. Last loop
            for col in aMatrix'Range (2) loop
               Real_IO.Put (aMatrix (row, col), 1, Places, 0);
               Put ("  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Float_Matrix called with invalid start or finish index.");
      end if;

   end Print_Float_Matrix_Formated;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array
     (Name  : String; anArray : NL_Arrays_And_Matrices.Integer_Array;
      Start : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
      Count : Natural := 0;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Integer'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 10 then
               New_Line;
               Count := 0;
            end if;
         end loop;
      else
         Put_Line
           ("Print_Integer_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Matrix (Name  : String; aMatrix : Integer_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0)
   is
      Last  : Integer;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aMatrix'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aMatrix'First and then Finish <= aMatrix'Last then
         for row in Start .. Last loop
            Put ("Row" & Integer'Image (row) & ":");
            for col in aMatrix'Range (2) loop
               Put (Integer'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
         end loop;
      else
         Put_Line
           ("Print_Integer_Matrix called with invalid start or finish index.");
      end if;

   end Print_Integer_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set) is
      use Encode_Utils.Int_Sets;
      Curs  : Cursor := theSet.First;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      while Has_Element (Curs) loop
         Put (Integer'Image (Element (Curs)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         Next (Curs);
      end loop;
      New_Line;

   end Print_Integer_Set;

   --  ------------------------------------------------------------------------

   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10) is
      Start      : Positive := Rows_Start;
      Last       : Positive := Rows_Last;
      Cols_Start : Positive := Rows_Start;
      Cols_Last  : Positive := Rows_Last;
   begin
      if Rows_Last > Params.Num_Rows then
         Last := Params.Num_Rows;
      end if;

      if Rows_Start > Rows_Last then
         Start := Rows_Last;
      end if;

      if Cols_Last > Params.Num_Cols then
         Cols_Last := Params.Num_Cols;
      end if;

      if Cols_Start > Cols_Last then
         Cols_Start := Cols_Last;
      end if;

      Put_Line (Name & ": ");
      Put_Line ("Size:" & Integer'Image (Params.Num_Rows) & " x" &
                  Integer'Image (Params.Num_Cols));

      Put_Line ("Coefficients:");
      for row in Start .. Last loop
         for col in Params.Coeff_Gradients'Range (2) loop
            Put (Float'Image (Params.Coeff_Gradients (row, col)) & " ");
         end loop;
         New_Line;
      end loop;

      Put_Line ("Intercepts:");
      for col in Cols_Start .. Cols_Last loop
         Put (Float'Image (Params.Intercept_Grads (col)) & " ");
      end loop;
      New_Line;
      New_Line;

   end Print_Parameters;

   --  ------------------------------------------------------------------------

   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set) is
      use Encode_Utils;
      UB_Strings_Curs : UB_String_Sets.Cursor := theSet.First;
      Count           : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;

      while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
         Put (To_String (UB_String_Sets.Element (UB_Strings_Curs)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         UB_String_Sets.Next (UB_Strings_Curs);
      end loop;
      New_Line;

   end Print_Unbounded_Set;

   --  ------------------------------------------------------------------------

end Neural_Printing;
