
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Utils;

procedure Test_S_Of_N_Creator is
   type D_10 is range 0 .. 9;
   package S_Of_3 is new Utils.S_Of_N_Creator (Sample_Size => 3,
                                               Item_Type => D_10);
   Routine_Name : constant String := "Test_S_Of_N_Creator ";
   Repetitions  : constant Positive := 100_000;
   Sample       : S_Of_3.Item_Array;  --  array (1 .. 3) of D_10;
   Result       : array (D_10) of Natural := (others => 0);
begin
   for count in 1 .. Repetitions loop
      --  get Sample
      for Dig in D_10 loop
         S_Of_3.Update (Dig);
      end loop;
      Sample := S_Of_3.Result;
      if count = Repetitions then
         Put_Line (Routine_Name & "last sample");
         for row in Sample'Range loop
            Put (D_10'Image (Sample (row)) & ", ");
         end loop;
         New_Line;
      end if;

      --  update current Result
      for Item in Sample'Range loop
         Result (Sample (Item)) := Result (Sample (Item)) + 1;
      end loop;
   end loop;

   --  output Result
   Put_Line (Routine_Name );
   for Dig in Result'Range loop
      Put (D_10'Image (Dig) & ":" & Natural'Image (Result (Dig)) &
             ", ");
   end loop;
   New_Line;

end Test_S_Of_N_Creator;
