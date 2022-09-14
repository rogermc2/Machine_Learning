
--  Based on scikit-learn/sklearn/utils/__init__.py
--  and scikit-learn/sklearn/utils/_random.pyx

--  with Ada.Assertions; use Ada.Assertions;
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

   function Sample_Without_Replacement (Population_Size, Sample_Size : Natural)
                                        return Integer_Array is
--        Routine_Name : constant String := "Utils.Sample_Without_Replacement ";
      N_Checked    : integer := 0;
      N_Selected   : integer := 0;
      u            : Float;
      Samples      : Integer_Array (1 .. Sample_Size);
   begin
      while N_Selected < Sample_Size loop
         u := abs (Maths.Random_Float);
         if Float (Population_Size - N_Checked) * u <
           Float (Sample_Size - N_Selected) then
            Samples (N_Selected) := N_Checked;
            N_Selected := N_Selected + 1;
         end if;
         N_Checked := N_Checked + 1;
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

end Utils;
