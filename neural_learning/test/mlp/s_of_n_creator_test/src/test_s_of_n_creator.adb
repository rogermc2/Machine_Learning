
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Utils;

--  From https://rosettacode.org/wiki/Knuth%27s_algorithm_S#Ada
--  Knuth's algorithm S is a method of randomly sampling n items from a set
--  of M items with equal probability where M >= n and M, the number of
--  items, is unknown until the end.
--  This means that equal probability sampling should be maintained for all
--  successive items > n as they become available (although the content of
--  Algorithm:
--  1. Select the first n items as the sample as they become available;
--  2. For the i-th item where i > n, have a random chance of n/i of keeping it.
--     If failing this chance, the sample remains unchanged.
--     otherwise, have it randomly (1/n) replace one of the previously selected
--     n items of the sample.
--  3. Repeat the 2nd step for any subsequent items.
--  Task:
--  Create a function s_of_n_creator that given a maximum sample size n
--  returns a function s_of_n that takes one parameter, item.
--  Function s_of_n when called with successive items returns an
--  equi-weighted random sample of up to n of its items so far each time it
--  is called, calculated using Knuths Algorithm S.
--  Test your functions by printing and showing the frequency of occurrences
--  of the selected digits from 100,000 repetitions of:
--  Use the s_of_n_creator with n == 3 to generate an s_of_n.
--  Call s_of_n with each of the digits 0 to 9 in order keeping the returned
--  three digits of its random sampling from its last call with argument
--  item = 9.
--  Note: A class taking n and generating a callable instance/function might
--  also be used.
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
