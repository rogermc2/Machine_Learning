--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Utilities;
with Label;
with NL_Types;
with Printing;

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
     (N_Samples            : Positive := 100;
      N_Features           : Positive := 20;
      N_Classes            : Positive := 5;
      N_labels             : Positive := 2;
      --  Length is the sum of the features (number of words for documents) and
      --  is drawn from a Poisson distribution with this expected value.
      Expected_Length      : Positive := 50;
      --  If Allow_Unlabeled is True then some instances might not belong to
      --  any class
      Allow_Unlabeled      : Boolean := True;
      --         Sparse               : Boolean := False;
      --         Return_Indicator     : Return_Indicator_Type := RI_Dense;
      --  If Return_Distributions is True then return the prior class
      --  probability and conditional probabilities of features given classes
      --  from which the data was drawn
      Return_Distributions : Boolean := False)
       return Multilabel_Classification is
      use NL_Types;
      Routine_Name : constant String :=
                       "Samples_Generator.Make_Multilabel_Classification ";
      Cum_P_C_List : Float_List;

      function Sample_Example (P_W_C : Real_Float_Matrix; Y : out Integer_List)
                                 return Integer_Array is
         Routine_Name : constant String :=
                       "Samples_Generator.Make_Multilabel_Classification." &
                       "Sample_Example ";
         use Maths;
         use Integer_Sorting;
         Y_Size    : Natural := N_Classes + 1;
         Prob      : NL_Types.Float_List;
         Num_Words : Natural := 0;
         Class     : Integer_List;
      begin
         --  pick a nonzero number of labels per document by rejection sampling
         while (not Allow_Unlabeled and Y_Size = 0) or Y_Size > N_Classes loop
            Y_Size := Poisson_Single (Float (N_labels));
         end loop;

         Assert (Y_Size > 0, Routine_Name & "Y_Size = 0");

         for index in 1 .. Y_Size - Natural (Y.Length) loop
            Prob.Append (abs (Maths.Random_Float));
         end loop;
         Printing.Print_Float_List (Routine_Name & "Cum_P_C_List", Cum_P_C_List);
         Printing.Print_Float_List (Routine_Name & "Prob", Prob);
         Put_Line (Routine_Name & "Prob length" &
                     Integer'Image (Integer (Prob.Length)));
         Put_Line (Routine_Name & "Cum_P_C_List length" &
                     Integer'Image (Integer (Cum_P_C_List.Length)));
         --  L410
         while Natural (Y.Length) /= Y_Size loop
            --  pick a class with probability P(c)
            Class := Classifier_Utilities.Search_Sorted_Float_List
              (Cum_P_C_List, Prob);
            Assert (Integer (Class.Length) > 0, Routine_Name &
                      "Sample_Example Class size = 0");
            Put_Line (Routine_Name & "Y.Length: " &
            Integer'Image (Integer (Y.Length)) & " Y size: " &
                        Integer'Image (Y_Size));
            for index in Class.First_Index .. Class.Last_Index loop
               if not Y.Contains (Class (index)) then
                  Y.Append (Class (index));
               else
                  Assert (False, Routine_Name & "Duplicate class " &
                            Integer'Image (Class (index)));
               end if;
            end loop;
         end loop;
         Put_Line (Routine_Name & "Y set");
         Sort (Y);
         Put_Line (Routine_Name & "Y sorted");

         --  L420 pick a nonzero document length by rejection sampling
         while Num_Words = 0 loop
            Num_Words := Poisson_Single (Float (Expected_Length));
         end loop;
         Put_Line (Routine_Name & "Num_Words set");

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
               Put_Line (Routine_Name & "Y empty");
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
               Put_Line (Routine_Name & "P_W_C_2 set");

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
      X              : Real_Float_Matrix (1 .. N_Samples, 1 .. N_Features)
        := (others => (others => 0.0));
      Y_Bool         : Boolean_Matrix (1 .. N_Samples, 1 .. N_Classes);
      Classification : Multilabel_Classification
        (N_Samples, N_Features, N_Classes, Return_Distributions);
      LB             : Label.Label_Binarizer;
      Y0             : Integer_Array (1 .. N_Classes);
      Y_List         : Integer_List;
   begin
      Put_Line (Routine_Name);
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

      --  L436
      for sample_index in 1 .. N_Samples loop
         Put_Line (Routine_Name & "L436 sample_index:" &
                     Integer'Image (sample_index));
         declare
            Sample_Y  : Integer_List;
            --  L437
            Words     : constant Integer_Array :=
                          Sample_Example (P_W_C, Sample_Y);
            X_Indices : Integer_List;
            X_Ind_Ptr : Integer_List;
         begin
            for index in Words'Range loop
               --  L438
               X_Indices.Append (Words (index));
               X_Ind_Ptr.Append (Natural (X_Indices.Length));
               --  L440
               Y_List.Append_Vector (Sample_Y);
            end loop;
            Put_Line (Routine_Name & "L440 done");

            --  csr_matrix((X_data, X_indices, X_indptr),
            --  shape=(n_samples, n_features)) is
            --  a representation where the column indices for row i are
            --  in X_indices[X_indptr[i] : X_indptr[i + 1]] and their
            --  related values are in X_data[X_indptr[i]:X_indptr[i+1]]

            for row in X'Range loop
               for col in X_Indices (X_Ind_Ptr (row)) ..
                 X_Indices (X_Ind_Ptr (row + 1)) loop
                  X (row, col) := 1.0;
               end loop;
            end loop;
            Put_Line (Routine_Name & "X set");
         end;
      end loop;

      --  L453
      for index in Y0'Range loop
         Y0 (index) := index - Y0'First;
      end loop;
      Label.Fit (LB, Y0);
      Y_Bool := Label.Transform (LB, Y_List);

      Classification.X := X;
      Classification.Y := To_Integer_Matrix (Y_Bool);
      if Return_Distributions then
         Classification.Class_Probability := P_C;
         Classification.Feature_Class_Probability := P_W_C;
      end if;
      return Classification;

   end Make_Multilabel_Classification;

end Samples_Generator;
