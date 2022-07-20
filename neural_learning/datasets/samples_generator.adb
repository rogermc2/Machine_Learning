--  Based on scikit-learn/sklearn/datasets/samples_generator.py

--  with Ada.Containers;

with Maths;

with Classifier_Utilities;
with Label;
with NL_Types;

package body Samples_Generator is

   --  Make_Multilabel_Classification generates random samples with multiple
   --  labels reflecting a bag of words drawn from a mixture of topics.
   --  The number of topics for each document is drawn from a Poisson
   --  distribution and the topics are drawn from a fixed random distribution.
   --  Similarly, the number of words is drawn from Poisson with words drawn
   --  from a multinomial where each topic defines a probability distribution
   --  over words.
   --  For each sample, the generative process is:
   --    1. pick the number of labels: n ~ Poisson(n_labels)
   --    2. n times, choose a class c: c ~ Multinomial(theta)
   --    3. pick the document length: k ~ Poisson(length)
   --    4. k times, choose a word: w ~ Multinomial(theta_c)
   --  Returns:
   --  X     : The generated samples as an n_samples x n_features matrix
   --  Y     : The label sets as an n_samples x n_classes matrix
   --  p_c   : An n_classes array of the probabilities of each class being drawn
   --  p_w_c : An n_features x n_classes matrix of the probability of each
   --          feature being drawn given each class
   function Make_Multilabel_Classification
     (N_Samples       : Positive := 100;
      N_Features      : Positive := 20;
      N_Classes       : Positive := 5;
      N_labels        : Positive := 2;
      --  Length is the sum of the features (number of words for documents) and
      --  is drawn from a Poisson distribution with this expected value.
      Expected_Length : Positive := 50;
      --  If Allow_Unlabeled is True then some instances might not belong to
      --  any class
      Allow_Unlabeled : Boolean := True)
      --         Sparse               : Boolean := False;
      --         Return_Indicator     : Return_Indicator_Type := RI_Dense;
      --  If Return_Distributions is True then return the prior class
      --  probability and conditional probabilities of features given classes
      --  from which the data was drawn
--        Return_Distributions : Boolean := False)
      return Multilabel_Classification is
      use NL_Types;
      Cum_P_C_List    : Float_List;

      function Sample_Example (P_W_C : Real_Float_Matrix; Y : out Integer_List)
                               return Integer_Array is
         use Maths;
         use Integer_Sorting;
         Y_Size    : Natural := N_Classes + 1;
         Prob      : NL_Types.Float_List;
         Num_Words : Natural := 0;
         Class     : Integer_List;
      begin
         --  pick a nonzero number of labels by rejection sampling
         while (not Allow_Unlabeled and Y_Size = 0) or Y_Size > N_Classes loop
            Y_Size := Poisson_Single (Float (N_labels));
         end loop;

         for index in 1 .. Y_Size - Natural (Y.Length) loop
            Prob.Append (abs (Maths.Random_Float));
         end loop;

         while Natural (Y.Length) /= Y_Size loop
            Class := Classifier_Utilities.Search_Sorted_Float_List
              (Cum_P_C_List, Prob);
            for index in Class.First_Index .. Class.Last_Index loop
               if not Y.Contains (Class (index)) then
                  Y.Append (Class (index));
               end if;
            end loop;
         end loop;
         Sort (Y);

         --  L420 pick a nonzero document length by rejection sampling
         while Num_Words = 0 loop
            Num_Words := Poisson_Single (Float (Expected_Length));
         end loop;

         declare
            use Float_Package;
            use Float_Sorting;
            Words           : Integer_Array (1 .. Num_Words);
            Word_List       : Integer_List;
            Cum_P_W_Sample  : Float_Array (1 .. N_Classes);
            Cum_Sample_List : Float_List;
            P_W_C_2         : Real_Float_Matrix (P_W_C'Range,
                                                 P_W_C'Range (2));
         begin
            if Y.Is_Empty then
               --  sample does'nt belong to a class so generate a
               --  noise word
               for index in 1 .. Num_Words loop
                  Word_List.Append
                    (abs (Maths.Random_Integer) * N_Features);
               end loop;

            else
               --  sample words with replacement from selected classes
               for row in P_W_C'Range loop
                  for col in P_W_C'Range (2) loop
                     P_W_C_2 (row, col) := P_W_C (row, Y (col));
                  end loop;
               end loop;

               for row in P_W_C_2'Range loop
                  Cum_P_W_Sample (row) := 0.0;
                  for col in P_W_C_2'Range (2) loop
                     Cum_P_W_Sample (row) :=
                       Cum_P_W_Sample (row) + P_W_C_2 (row, col);
                  end loop;
               end loop;
               Cum_P_W_Sample := Cumulative_Sum (Cum_P_W_Sample);
               for col in Cum_P_W_Sample'Range loop
                  Cum_Sample_List.Append
                    (Cum_P_W_Sample (col) /
                         Cum_P_W_Sample (Cum_P_W_Sample'Last));
               end loop;
               Sort (Cum_Sample_List);

               Prob.Clear;
               for index in 1 .. Num_Words loop
                  Prob.Append (abs (Maths.Random_Float));
               end loop;
               Word_List := Classifier_Utilities.Search_Sorted_Float_List
                 (Cum_Sample_List, Prob);
            end if;

            for index in 1 .. Num_Words loop
               Words (index) := Word_List (index);
            end loop;
            return Words;
         end;

      end Sample_Example;

      P_C            : Float_Array (1 .. N_Classes);
      Cum_P_C        : Float_Array (1 .. N_Classes);
      P_W_C          : Real_Float_Matrix (1 .. N_Features, 1 .. N_Classes);
      X              : Real_Float_Matrix (1 .. N_Samples, 1 .. N_Features);
      Y              : Integer_Matrix (1 .. N_Samples, 1 .. N_Classes);
      Y_Bool         : Boolean_Matrix (1 .. N_Samples, 1 .. N_Classes);
      Classification : Multilabel_Classification
        (N_Samples, N_Features, N_Classes);
      LB             : Label.Label_Binarizer;
      Y_List         : Integer_List;
   begin
      for index in P_C'Range loop
         P_C (index) := abs (Maths.Random_Float);
      end loop;
      P_C := Normalize (P_C);
      Cum_P_C := Cumulative_Sum (P_C);

      for row in P_W_C'Range loop
         for col in P_W_C'Range (2) loop
            P_W_C (row, col) := abs (Maths.Random_Float);
         end loop;
      end loop;
      P_W_C := Normalize_Rows (P_W_C);

      for index in Cum_P_C'Range loop
         Cum_P_C_List.Append (Cum_P_C (index));
      end loop;

      for index in 1 .. N_Samples loop
         declare
            Sample_Y  : Integer_List;
            Words     : constant Integer_Array :=
                          Sample_Example (P_W_C, Sample_Y);
            X_Indices : Integer_List;
         begin
            for index in Words'Range loop
               X_Indices.Append (Words (index));
               Y_List.Append_Vector (Sample_Y);
            end loop;

            declare
               X_Data : Float_Array (1 .. Positive (X_Indices.Length))
                 := (others => 1.0);
               X      : array (X_Data'Range, 1 .. X_Indices.Length)
                 of Float;
            begin
               null;
            end;
         end;
      end loop;

      Label.Fit (LB, Y);
      Y_Bool := Label.Transform (LB, Y);

      Classification.X := X;
      return Classification;

   end Make_Multilabel_Classification;

end Samples_Generator;
