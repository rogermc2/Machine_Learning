
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Support is

   function Almost_Equal (A, B : Real_Float_Matrix; Accuracy : Integer)
                          return Boolean is
      use Real_Float_Arrays;
      Diff   : constant Real_Float_Matrix := abs (A - B);
      Result : Boolean := True;
   begin
      for row in Diff'Range loop
         for col in Diff'Range (2) loop
            Result := Result and Diff (row, col) < 10.0 ** Accuracy;
         end loop;
      end loop;

      return Result;

   end Almost_Equal;

   --  ------------------------------------------------------------------------

   function Almost_Equal (A, B : Real_Float_Vector; Accuracy : Integer)
                          return Boolean is
      use Real_Float_Arrays;
      Diff   : constant Real_Float_Vector := abs (A - B);
      Result : Boolean := True;
   begin
      for row in Diff'Range loop
         Result := Result and Diff (row) < 10.0 ** Accuracy;
      end loop;

      return Result;

   end Almost_Equal;

   --  ------------------------------------------------------------------------

   function Almost_Equal (A, B     : Stochastic_Optimizers.Parameters_Record;
                          Accuracy : Integer) return Boolean is
   begin

      return Almost_Equal (A.Coeff_Gradients, B.Coeff_Gradients, Accuracy) and
        Almost_Equal (A.Intercept_Grads, B.Intercept_Grads, Accuracy);

   end Almost_Equal;

   --  ------------------------------------------------------------------------

   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix) is
   begin
      Put_Line (Name & ": ");
      for row in aMatrix'Range loop
         for col in aMatrix'Range (2) loop
            Put (Integer'Image (aMatrix (row, col)) & "  ");
         end loop;
         New_Line;
      end loop;

   end Print_Binary_Matrix;

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

   procedure Print_Float_Matrix
     (Name  : String; aMatrix : Real_Float_Matrix) is
   begin
      Put_Line (Name & ": ");
      for row in aMatrix'Range loop
         for col in aMatrix'Range (2) loop
            Put (Float'Image (aMatrix (row, col)) & "  ");
         end loop;
         New_Line;
      end loop;

   end Print_Float_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Vector (Name : String; Vec : Real_Float_Vector) is
   begin
      Put_Line (Name & ": ");
      for row in Vec'Range loop
         Put (Float'Image (Vec (row)) & "  ");
         New_Line;
      end loop;

   end Print_Float_Vector;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array (Name  : String; anArray : Integer_Array;
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
            Put (Integer'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line ("Print_Integer_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_List (Name  : String;
                                 aList : NL_Types.Integer_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in aList.First_Index .. aList.Last_Index loop
         Put (Integer'Image (aList (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Integer_List;

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

   procedure Print_Unbound_List (Name : String;
                                 UB   : NL_Types.Unbounded_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in UB.First_Index .. UB.Last_Index loop
         Put (To_String (UB (Index)) & "  ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Unbound_List;

   --  ------------------------------------------------------------------------

   procedure Print_Unbound_Matrix (Name : String;
                                   UB   : Unbounded_String_Matrix) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for row in UB'Range loop
         for col in UB'Range (2) loop
            Put (To_String (UB (row, col)) & "  ");
            Count := Count + 1;
            if Count > 10 then
               New_Line;
               Count := 1;
            end if;
         end loop;
         New_Line;
      end loop;

   end Print_Unbound_Matrix;

   --  ------------------------------------------------------------------------

end Test_Support;
