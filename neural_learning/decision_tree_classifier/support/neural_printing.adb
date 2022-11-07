
with Ada.Text_IO; use Ada.Text_IO;

package body Neural_Printing is

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
