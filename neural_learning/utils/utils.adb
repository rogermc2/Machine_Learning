
--  Based on scikit-learn/sklearn/utils/__init__.py
--  and scikit-learn/sklearn/utils/_random.pyx

with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Utils is

   --     function Sample_Without_Replacement_With_Reservoir_Sampling
   --       (Population_Size, Num_Samples : Positive) return Integer_Array;

   --  -------------------------------------------------------------------------
   --  L695 Gen_Batches returns a list of slice lists
   function Gen_Batches (Num_To_Slice, Batch_Size : Positive;
                         Min_Batch_Size           : Natural := 0)
                         return Slices_List is
      --        Routine_Name : constant String := "Utils.Gen_Batches ";
      Start       : Positive := 1;
      Last        : Positive;
      aSlice      : Slice_Record;
      Slices      : Slices_List;
   begin
      for index in 1 .. Num_To_Slice / Batch_Size loop
         Last := Start + Batch_Size - 1;
         if Last + Min_Batch_Size <= Num_To_Slice then
            for item in Start .. Last loop
               aSlice := (Start, Last);
            end loop;
            Start := Last + 1;
         else
            for item in Start .. Num_To_Slice loop
               aSlice := (Start, Last);
            end loop;
         end if;
         Slices.Append (aSlice);
      end loop;

      return Slices;

   end Gen_Batches;

   --  -------------------------------------------------------------------------
   --  Knuth's algorithm S
   function Sample_Without_Replacement (Population_Size, Sample_Size : Natural)
                                        return Integer_Array is
      Routine_Name : constant String := "Utils.Sample_Without_Replacement ";
      N_Checked    : integer := 0;
      N_Selected   : integer := 0;
      u            : Float;
      Samples      : Integer_Array (1 .. Sample_Size);
   begin
      while N_Checked < Population_Size and then
        N_Selected < Sample_Size loop
         N_Checked := N_Checked + 1;
         u := abs (Maths.Random_Float);
         if Float (Population_Size - N_Checked - 1) * u <
           Float (Sample_Size - N_Selected) then
            Assert (N_Selected <= Sample_Size, Routine_Name & "N_Selected" &
                      Integer'Image (N_Selected) & ", Sample_Size" &
                      Integer'Image (Sample_Size));
            N_Selected := N_Selected + 1;
            Samples (N_Selected) := N_Checked;
         end if;
      end loop;

      return Samples;

   end Sample_Without_Replacement;

   --  ------------------------------------------------------------------------
   --  _random.pyx L226 Sample_Without_Replacement selects n_samples integers
   --   from the set [0, n_population) without replacement.
   --     function Sample_Without_Replacement (Population_Size, Num_Samples : Positive)
   --                                          return Integer_Array is
   --        Routine_Name : constant String := "Utils.Sample_Without_Replacement ";
   --        Ratio        : constant Float :=
   --                         Float (Num_Samples) / Float (Population_Size);
   --        Result       : Integer_Array (1 .. Num_Samples);
   --     begin
   --        Assert (Num_Samples <= Population_Size, Routine_Name &
   --                  "Num_Samples should not be greater than Population_Size.");
   --
   --        if Ratio > 0.01 and Ratio < 0.99 then
   --           null;
   --        else
   --           Result := Sample_Without_Replacement_With_Reservoir_Sampling
   --             (Population_Size, Num_Samples);
   --        end if;
   --
   --        return Result;
   --
   --     end Sample_Without_Replacement;

   --  ------------------------------------------------------------------------
   --  _random.pyx L161 Sample_Without_Repalcement_With_Reservoir_Sampling
   --  selects n_samples integers from the set [0, n_population) without
   --  replacement.
   --     function Sample_Without_Replacement_With_Reservoir_Sampling
   --       (Population_Size, Num_Samples : Positive) return Integer_Array is
   --        Routine_Name : constant String := "Utils.Sample_Without_Replacement ";
   --        Rand_Int     : Integer := Maths.Random_Integer (0, 1);
   --        Result       : Integer_Array (1 .. Num_Samples);
   --     begin
   --        Assert (Num_Samples <= Population_Size, Routine_Name &
   --                  "Num_Samples should not be greater than Population_Size.");
   --        for index in 1 .. Num_Samples loop
   --           Result (index) := index - 1;
   --        end loop;
   --
   --        for index in Num_Samples .. Population_Size loop
   --           Rand_Int := Maths.Random_Integer (0, index + 1);
   --           if Rand_Int < Num_Samples then
   --              Result (Rand_Int + 1) := index;
   --           end if;
   --        end loop;
   --
   --        return Result;
   --
   --     end Sample_Without_Replacement_With_Reservoir_Sampling;

   --  ------------------------------------------------------------------------

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
   --  1. Create a function S_Of_N_Creator that given n, the maximum sample size,
   --     returns a function S_Of_N that takes one parameter, item.
   --  2. Function S_Of_N when called with successive items returns an
   --     equi-weighted random sample of up to n of its items so far each time it
   --     is called, calculated using Knuths Algorithm S.
   --  The Ada example, instead of defining a function S_of_N_Creator, defines a
   --  generic packgage with that name.
   --  The generic parameters are N (=Sample_Size) and the type of the items to be
   --  sampled:
   package body S_Of_N_Creator is

      package Rand_Float renames Ada.Numerics.Float_Random;
      F_Gen : Rand_Float.Generator;

      package Rand_Discrete is new Ada.Numerics.Discrete_Random (Index_Type);
      D_Gen : Rand_Discrete.Generator;

      Item_Count : Natural := 0; -- global counter
      Sample     : Item_Array;   -- also used globally

      procedure Update (New_Item : Item_Type) is
      begin
         Item_Count := Item_Count + 1;
         if Item_Count <= Sample_Size then
            --  select the first Sample_Size items as the sample
            Sample (Item_Count) := New_Item;
         else
            --  for the I-th item where I > Sample_Size:
            --  Sample_Size/I is the chance of keeping it
            if (Float (Sample_Size) / Float (Item_Count)) >
              Rand_Float.Random (F_Gen) then
               --  randomly (1 / Sample_Size) replace one of the items of the
               --  sample
               Sample (Rand_Discrete.Random (D_Gen)) := New_Item;
            end if;
         end if;

      end Update;

      function Result return Item_Array is
      begin
         Item_Count := 0; -- ready to start another run
         return Sample;
      end Result;

   begin
      --  at package instantiation, initialize rnd-generators
      Rand_Discrete.Reset (D_Gen);
      Rand_Float.Reset (F_Gen);

   end S_Of_N_Creator;

   --  ------------------------------------------------------------------------

end Utils;
