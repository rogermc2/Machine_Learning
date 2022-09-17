--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with System.Aux_DEC;
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utils;

with Classifier_Utilities;
with Label;
--  with Test_Support;
with Utilities;

package body Samples_Generator is

   function Generate_Hypercube (Samples, Dimensions : Positive)
                                 return Binary_Matrix;
   procedure Randomly_Shift_And_Scale
     (X     : in out Real_Float_Matrix; N_Features : Positive;
      Shift : Shift_List; Scale : Scale_List; Class_Sep : Float);
   procedure Randomly_Replace_Labels (Y         : in out Integer_Array;
                                      N_Classes : Positive; Flip_Y : Float);
   procedure Repeat_Features
     (X                                      : in out Real_Float_Matrix;
      N_Informative, N_Redundant, N_Repeated : Natural);
   procedure Shuffle_Data (X          : in out Real_Float_Matrix;
                           Y          : in out Integer_Array;
                           N_Features : Positive);

   --  ------------------------------------------------------------------------
   --  L222  Build the polytope whose vertices become cluster centroids
   procedure Create_Centroids (X                     : in out Real_Float_Matrix;
                               Y                     : in out Integer_Array;
                               N_Clusters,
                               N_Informative,
                               N_Classes             : Natural;
                               N_Samples_Per_Cluster : Integer_Array;
                               Class_Sep             : Float;
                               Hypercube             : Boolean) is
      use Real_Float_Arrays;
--        Routine_Name : constant String := "Samples_Generator.Create_Centroids ";
      Centroids    : Real_Float_Matrix := To_Real_Float_Matrix
        (Generate_Hypercube (N_Clusters, N_Informative));
      Centroid     : Real_Float_Vector (Centroids'Range (2));
      Stop         : Natural := 0;
      Start        : Positive := 1;
   begin
      Centroids := 2.0 * Class_Sep * Centroids - Class_Sep;
      if not Hypercube then
         for row in X'Range loop
            for col in 1 .. N_Informative loop
               Centroids (row, col) :=
                 abs (Maths.Random_Float) * Centroids (row, col);
            end loop;
         end loop;
      end if;

      --  L232 Initially draw informative features from the standard normal
      for row in X'Range loop
         for col in 1 .. N_Informative loop
            X (row, col) := Maths.Normal_Distribution;
         end loop;
      end loop;

      --  L235 Create clusters
      for row in Centroids'Range loop
         Start := Stop + 1;
         Stop := Stop + N_Samples_Per_Cluster (row);
         for y_row in Start .. Stop loop
            --  assign label
            Y (y_row) := row mod N_Classes;
         end loop;

         --  L240
         declare
            X_k  : Real_Float_Matrix (1 .. Stop - Start + 1,
                                      1 .. N_Informative);
            A    : Real_Float_Matrix (1 .. N_Informative,
                                      1 .. N_Informative);
         begin
            --  slice a view X_k of the cluster
            for x_row in Start .. Stop loop
               for x_col in 1 .. N_Informative loop
                  X_k (x_row - Start + 1, x_col) := X (x_row, x_col);
               end loop;
            end loop;

            --  introduce random covariance
            for a_row in A'Range loop
               for a_col in A'Range (2) loop
                  A (a_row, a_col) := Maths.Random_Float;
               end loop;
            end loop;
            X_k := X_k * A;

            --  shift the cluster to a vertex
            for c_col in Centroid'Range loop
               Centroid (c_col) := Centroids (row, c_col);
            end loop;
            X_k := X_k + Centroid;

            for x_row in Start .. Stop loop
               for x_col in 1 .. N_Informative loop
                  X (x_row, x_col) := X_k (x_row - Start + 1, x_col);
               end loop;
            end loop;

         end;  --  declare block
      end loop;

   end Create_Centroids;

   --  ------------------------------------------------------------------------

   procedure Create_Redundent_Features
     (X : in out Real_Float_Matrix; N_Informative, N_Redundant : Natural) is
      use Real_Float_Arrays;
      B  : Real_Float_Matrix (1 .. N_Informative, 1 .. N_Redundant);
      X1 : Real_Float_Matrix (X'Range, 1 .. N_Informative);
   begin
      for row in B'Range loop
         for col in B'Range (2) loop
            B (row, col) := Maths.Random_Float;
         end loop;
      end loop;

      for row in X1'Range loop
         for col in X1'Range (2) loop
            X1 (row, col) := X (row, col);
         end loop;
      end loop;

      X1 := X1 * B;

      for row in X1'Range loop
         for col in X1'Range (2)  loop
            X (row, N_Informative + col) := X1 (row, col);
         end loop;
      end loop;

   end Create_Redundent_Features;

   --  ------------------------------------------------------------------------

   procedure Fill_Useless_Features (X         : in out Real_Float_Matrix;
                                    N_Useless : Natural) is
      Useless : Real_Float_Matrix (X'Range, 1 .. N_Useless);
   begin
      for row in Useless'Range loop
         for col in Useless'Range (2) loop
            Useless (row, col) := Maths.Normal_Distribution;
         end loop;
      end loop;

      for row in Useless'Range loop
         for col in Useless'Range (2) loop
            X (row, X'Last (2) - N_Useless + col) := Useless (row, col);
         end loop;
      end loop;

   end Fill_Useless_Features;

   --  ------------------------------------------------------------------------
   --  _generate_hypercube(samples, dimensions, rng) returns distinct binary
   --  samples of length dimensions.
   --  Sample_Without_Replacement (n_population, n_samples):
   --  n_population : the size of the set to sample from (samples).
   --  n_samples : the number of integers to sample (dimensions).
   function Generate_Hypercube (Samples, Dimensions : Positive)
                                 return Binary_Matrix is
      use System.Aux_DEC;
--        Routine_Name : constant String :=
--                         "Samples_Generator.Generate_Hypercube ";
      Dims         : constant Positive := 2 ** Dimensions;
      Out_Vec      : constant Integer_Array :=
                       Utils.Sample_Without_Replacement (Dims, Samples);
      Bits         : Bit_Array_32;
      Hypercube    : Binary_Matrix (1 .. Samples, 1 .. Dimensions) :=
                       (others => (others => 0));
   begin
      for row in Hypercube'Range loop
         Bits := To_Bit_Array_32 (Unsigned_32 (Out_Vec (row)));
         for col in Hypercube'Range (2) loop
            if Bits (col - 1) then
               Hypercube (row, col) := 1;
            end if;
         end loop;
      end loop;

      return Hypercube;

   end Generate_Hypercube;

   --  -------------------------------------------------------------------------
   --  Make_Classification generates a random n-class classification problem
   --  which initially creates clusters of points normally distributed (std=1)
   --  about vertices of an n_informative-dimensional hypercube with sides of
   --  length 2*class_sep and assigns an equal number of clusters to each class.
   --  It introduces interdependence between these features and adds various
   --  types of further noise to the data.
   function Make_Classification
     (N_Samples            : Positive := 100;
      N_Features           : Positive := 20;
      N_Informative        : Positive := 2;
      N_Redundant          : Positive := 2;
      N_Repeated           : Natural := 0;
      N_Classes            : Positive := 2;
      N_Clusters_Per_Class : Positive := 2;
      Weights              : in out NL_Types.Float_List;
      Flip_Y               : Float := 0.01;
      Class_Sep            : Float := 1.0;
      Hypercube            : Boolean := True;
      Shift                : Shift_List := Default_Shift_List;
      Scale                : Scale_List := Default_Scale_List;
      Shuffle              : Boolean := True)
     --        Random_State         : Boolean := False)
       return Classification_Test_Data is
      use NL_Types.Float_Package;
      Routine_Name          : constant String :=
                                "Samples_Generator.Make_Classification ";
      N_Clusters            : constant Positive :=
                                N_Classes * N_Clusters_Per_Class;
      N_Useless             : constant Positive
        := N_Features - N_Informative - N_Redundant - N_Repeated;
      Sum                   : Float := 0.0;

      N_Samples_Per_Cluster : Integer_Array (1 .. N_Clusters);
      N_Cluster_Samples     : Natural := 0;
      X                     : Real_Float_Matrix (1 .. N_Samples,
                                                 1 .. N_Features) :=
                                (others => (others => 0.0));
      Y                     : Integer_Array (1 .. N_Samples) :=
                                (others => 0);
      Classification        : Classification_Test_Data (N_Samples, N_Features,
                                                        N_Classes);
   begin
      Assert (N_Informative + N_Redundant + N_Repeated <= N_Features,
              Routine_Name & "the total number of informative, redundant " &
                " and repeated features must be less than number of " &
                " features");

      Assert (Float (N_Informative) >= Maths.Float_Math_Functions.Log
              (Float (N_Classes * N_Clusters_Per_Class), 2.0), Routine_Name &
                "N_Classes * N_Clusters_Per_Class must be smaller than or " &
                "equal to 2 ^ N_Informative");

      --  L192
      if Is_Empty (Weights) then
         for index in 1 .. N_Classes loop
            Weights.Append (1.0 / Float (N_Classes));
         end loop;
      else
         Assert (Integer (Weights.Length) = N_Classes or else
                 Integer (Weights.Length) = N_Classes - 1, Routine_Name &
                   "the specified number of weights is incompatible " &
                   "with the number of classes.");
         if Integer (Weights.Length) = N_Classes - 1 then
            for index in Weights.First_Index .. Weights.Last_Index loop
               Sum := Sum + Weights (index);
            end loop;
            Weights.Append (1.0 - Sum);
         end if;
      end if;

      --  Distribute samples among clusters by weight
      for index in N_Samples_Per_Cluster'Range loop
         N_Samples_Per_Cluster (index) :=
           Integer (Float (N_Samples) *
                        Weights (index mod Integer (Weights.Length) + 1) /
                      Float (N_Clusters_Per_Class));
      end loop;

      for index in N_Samples_Per_Cluster'Range loop
         N_Cluster_Samples :=
           N_Cluster_Samples + N_Samples_Per_Cluster (index);
      end loop;

      for index in 1 .. N_Samples - N_Cluster_Samples loop
         N_Samples_Per_Cluster (index mod N_Clusters) :=
           N_Samples_Per_Cluster (index mod N_Clusters) + 1;
      end loop;

      --  L222 Build the polytope whose vertices become cluster centroids
      Create_Centroids (X, Y, N_Clusters, N_Informative, N_Classes,
                        N_Samples_Per_Cluster, Class_Sep, Hypercube);

      --  L247  Create redundant features
      if N_Redundant > 0 then
         Create_Redundent_Features (X, N_Informative, N_Redundant);
      end if;

      --  L247  Repeat some features
      if N_Repeated > 0 then
         Repeat_Features (X, N_Informative, N_Redundant, N_Repeated);
      end if;

      if N_Useless > 0 then
         Fill_Useless_Features (X, N_Useless);
      end if;

      if Flip_Y > 0.0 then
         Randomly_Replace_Labels (Y, N_Classes, Flip_Y);
      end if;

      Randomly_Shift_And_Scale (X, N_Features, Shift, Scale, Class_Sep);

      if Shuffle then
         Shuffle_Data (X, Y, N_Features);
      end if;

      Classification.X := X;

      for row in Y'Range loop
         Classification.Y (row, 1) := Y (row);
      end loop;

      return Classification;

   end Make_Classification;

   --  -------------------------------------------------------------------------
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
      N_Labels             : Positive := 2;
      --  Length is the sum of the features (number of words for documents) and
      --  is drawn from a Poisson distribution with this expected value.
      Expected_Length      : Positive := 50;
      --  If Allow_Unlabeled is True then some instances might not belong to
      --  any class
      Allow_Unlabeled      : Boolean := True;
      --         Return_Indicator     : Return_Indicator_Type := RI_Dense;
      --  If Return_Distributions is True then return the prior class
      --  probability and conditional probabilities of features given classes
      --  from which the data was drawn
      Return_Distributions : Boolean := False)
       return Multilabel_Classification_Test_Data is
      use NL_Types;
--        Routine_Name : constant String :=
--                         "Samples_Generator.Make_Multilabel_Classification ";
      Cum_P_C_List : Float_List;

      --  Sample_Example returns one sample example word vector and Y vector
      function Sample_Example
        (P_W_C_Ex : Real_Float_Matrix; Y_Gen : out Integer_List)
           return Integer_Array is
         Routine_Name : constant String :=
                          "Samples_Generator.Make_Multilabel_Classification." &
                          "Sample_Example ";
         use Maths;
         --           use Integer_Sorting;
         Num_Classes  : constant Positive := P_W_C_Ex'Length;
         Y_Size       : Natural := Num_Classes + 1;
         Prob         : NL_Types.Float_List;
         Num_Words    : Natural := 0;
         Class        : Integer_List;
      begin
         --  L403 pick a nonzero number of labels per document by rejection
         --  sampling
         while (not Allow_Unlabeled and Y_Size = 0) or Y_Size > Num_Classes loop
            --  L406
            Y_Size := Poisson_Single (Float (N_labels));
         end loop;
         --           Put_Line (Routine_Name & " L407 Y_Size:" & Integer'Image (Y_Size));

         for index in 1 .. Y_Size loop
            Prob.Append (abs (Maths.Random_Float));
         end loop;

         --           Printing.Print_Float_List (Routine_Name &
         --                                        "Cum_P_C_List", Cum_P_C_List);
         --           Printing.Print_Float_List (Routine_Name & "Prob", Prob);
         --           Put_Line (Routine_Name & "Prob length" &
         --                       Integer'Image (Integer (Prob.Length)));
         --           Put_Line (Routine_Name & "Cum_P_C_List length" &
         --                       Integer'Image (Integer (Cum_P_C_List.Length)));
         --  L410 pick n classes
         while Natural (Y_Gen.Length) /= Y_Size loop
            --              Put_Line (Routine_Name & "L410 Y_Gen.Length /= Y_Size");
            --              Put_Line (Routine_Name & "L410 Y_Gen.Length: " &
            --                          Integer'Image (Integer (Y_Gen.Length)) &
            --                          " Y size: " & Integer'Image (Y_Size));
            --  L412 pick a class with probability P(c)
            Class := Classifier_Utilities.Search_Sorted_Float_List
              (Cum_P_C_List, Prob);
            --              Printing.Print_Integer_List (Routine_Name & "L412 Class", Class);
            Assert (Integer (Class.Length) > 0, Routine_Name &
                      "L412 Class size = 0");
            for index in Class.First_Index .. Class.Last_Index loop
               Y_Gen.Append (Class (index));
            end loop;
         end loop;
         --           Printing.Print_Integer_List (Routine_Name & "L420 Y_Gen", Y_Gen);

         --  L420 pick a nonzero document length by rejection sampling
         while Num_Words = 0 loop
            Num_Words := Poisson_Single (Float (Expected_Length));
         end loop;
         --           Put_Line (Routine_Name & "L424 Num_Words: " &
         --                       Integer'Image (Num_Words));

         declare
            use Float_Package;
            use Float_Sorting;
            Words                                : Integer_Array (1 .. Num_Words);
            Word_List                            : Integer_List;
            Cum_P_W_Sample                       : Float_Array (1 .. Num_Classes);
            Cum_Sample_List                      : Float_List;
            P_W_C_2                              : Real_Float_Matrix
              (P_W_C_Ex'Range, P_W_C_Ex'Range (2)) := (others => (others => 0.0));
         begin
            --  L425 generate a document of length n_words
            if Y_Gen.Is_Empty then
               --  sample does'nt belong to a class so generate a
               --  noise word
               for index in 1 .. Num_Words loop
                  Word_List.Append (Maths.Random_Integer (1, N_Features));
               end loop;

            else
               --  L431 sample words with replacement from selected classes
               --  cumulative_p_w_sample =
               --  p_w_c.take(y, axis=1).sum(axis=1).cumsum()
               --  y: column indices of values to be fetched from p_w_c
               for row in 1 .. N_Features loop
                  --                    Put_Line (Routine_Name & "L431 Y_Gen Row:" &
                  --                                Integer'Image (row));
                  for col in Y_Gen.First_Index .. Y_Gen.Last_Index loop
                     --                       Put_Line (Routine_Name & "L431 Y_Gen col:" &
                     --                                   Integer'Image (col));
                     Assert (Y_Gen (col) <= P_W_C_Ex'Length (2),
                             Routine_Name & "L431 Y_Gen col " &
                               Integer'Image (Y_Gen (col)) &
                               " > PWC_Ex'Length (2)"  &
                               Integer'Image (P_W_C_Ex'Length (2)));
                     Assert (col <= P_W_C_2'Length (2),
                             Routine_Name & "L431 Y_Gen col " &
                               Integer'Image (col) & " > num PWC_2 cols"  &
                               Integer'Image (P_W_C_2'Length (2)));
                     P_W_C_2 (row, col) := P_W_C_Ex (row, Y_Gen (col));
                  end loop;
               end loop;

               for row in P_W_C_2'Range loop
                  Cum_P_W_Sample (row) := 0.0;
                  for col in P_W_C_2'Range (2) loop
                     Cum_P_W_Sample (row) :=
                       Cum_P_W_Sample (row) + P_W_C_2 (row, col);
                  end loop;
               end loop;

               --  L432
               Cum_P_W_Sample := Cumulative_Sum (Cum_P_W_Sample);
               --  L433
               for col in Cum_P_W_Sample'Range loop
                  Cum_Sample_List.Append
                    (Cum_P_W_Sample (col) /
                         Cum_P_W_Sample (Cum_P_W_Sample'Last));
               end loop;
               Sort (Cum_Sample_List);
               --                 Printing.Print_Float_List
               --                   (Routine_Name & " L433 Cum_Sample_List", Cum_Sample_List);

               --  L434
               Prob.Clear;
               for index in 1 .. Num_Words loop
                  Prob.Append (abs (Maths.Random_Float));
               end loop;

               --  L430
               Word_List := Classifier_Utilities.Search_Sorted_Float_List
                 (Cum_Sample_List, Prob);
            end if;
            --              Printing.Print_Integer_List
            --                (Routine_Name & "L430 Word_List", Word_List);

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
      Classification : Multilabel_Classification_Test_Data
        (N_Samples, N_Features, N_Classes, Return_Distributions);
      LB             : Label.Label_Binarizer;
      Y              : Array_Of_Integer_Lists (1 .. N_Samples);
      X_Indices      : Integer_List;
      X_Ind_Ptr      : Array_Of_Integer_Lists (1 .. N_Samples);
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

      --  L436
      for sample_index in 1 .. N_Samples loop
         --           Put_Line (Routine_Name & "L436 sample_index:" &
         --                       Integer'Image (sample_index));
         declare
            Sample_Y  : Integer_List;
            --  L437
            Words     : constant Integer_Array :=
                          Sample_Example (P_W_C, Sample_Y);
         begin
            --  L438
            X_Indices := Classifier_Utilities.To_Integer_List (Words);
            X_Ind_Ptr (sample_index) := X_Indices;
            --  L440
            Y (sample_index) := Sample_Y;
         end;
      end loop;

      --  L441
      --  csr_matrix((X_data, X_indices, X_indptr),
      --  shape=(n_samples, n_features)) is
      --  a representation where the column indices for row i are
      --  in X_indices[X_indptr[i] : X_indptr[i + 1]] and their
      --  related values are in X_data[X_indptr[i]:X_indptr[i+1]]

      --  sum_duplicates
      for row in X_Ind_Ptr'Range loop
         for sp_col in X_Ind_Ptr (row).First_Index ..
           X_Ind_Ptr (row).Last_Index loop
            X (row, X_Ind_Ptr (row).Element (sp_col)) :=
              X (row, X_Ind_Ptr (row).Element (sp_col)) + 1.0;
         end loop;
      end loop;

      --  L453
      --        Printing.Print_Array_Of_Integer_Lists (Routine_Name & "L453 Y", Y, 1, 4);
      Label.Fit (LB, Y);
      declare
         Y_Bin : constant Binary_Matrix := Label.Transform (LB, Y);
      begin
         --           Put_Line (Routine_Name & "L453 Y_Bool size :" &
         --                       Integer'Image (Y_Bool'Length) & " x" &
         --                       Integer'Image (Y_Bool'Length (2)));
         --           Put_Line (Routine_Name & "L453 Classification.Y length:" &
         --                       Integer'Image (Classification.Y'Length) & " x" &
         --                       Integer'Image (Classification.Y'Length (2)));
         Classification.Y := Y_Bin;
      end;

      Classification.X := X;

      if Return_Distributions then
         Classification.Class_Probability := P_C;
         Classification.Feature_Class_Probability := P_W_C;
      end if;

      return Classification;

   end Make_Multilabel_Classification;

   --  ------------------------------------------------------------------------
   --  Flip_Y is the fraction of samples whose class is assigned randomly
   procedure Randomly_Replace_Labels (Y         : in out Integer_Array;
                                      N_Classes : Positive; Flip_Y : Float) is
      Flip_Mask : Boolean_Array (Y'Range);
   begin
      for row in Flip_Mask'Range loop
         Flip_Mask (row) := abs (Maths.Random_Float) < Flip_Y;
      end loop;

      for row in Y'Range loop
         if Flip_Mask (row) then
            Y (row) := Maths.Random_Integer (1, N_Classes);
         end if;
      end loop;

   end Randomly_Replace_Labels;

   --  ------------------------------------------------------------------------

   procedure Randomly_Shift_And_Scale
     (X     : in out Real_Float_Matrix; N_Features : Positive;
      Shift : Shift_List;  Scale : Scale_List; Class_Sep : Float) is
      use Ada.Containers;
      Routine_Name : constant String :=
                       "Samples_Generator.Randomly_Shift_And_Scale ";
      L_Shift      : Shift_List := Shift;
      L_Scale      : Scale_List := Scale;
   begin
      if Shift.Is_Empty then
         for index in 1 .. N_Features loop
            L_Shift.Append (Class_Sep * Maths.Random_Float);
         end loop;

      elsif Shift.Length = 1 then
         for index in 2 .. N_Features loop
            L_Shift.Append (Shift (1));
         end loop;
      else
         Assert (Positive (Shift.Length) = N_Features, Routine_Name &
                   "Shift length is not N_Features");
      end if;

      if Scale.Is_Empty then
         for index in 1 .. N_Features loop
            L_Scale.Append (Float (Maths.Random_Integer (1, 100)));
         end loop;

      elsif Scale.Length = 1 then
         for index in 2 .. N_Features loop
            L_Scale.Append (Scale (1));
         end loop;
      else
         Assert (Positive (Scale.Length) = N_Features, Routine_Name &
                   "Scale length is not N_Features");
      end if;

      for row in X'Range loop
         for col in X'Range (2) loop
            X (row, col) := L_Scale (col) * (X (row, col) + L_Shift (col));
         end loop;
      end loop;

   end Randomly_Shift_And_Scale;

   --  ------------------------------------------------------------------------

   procedure Repeat_Features (X          : in out Real_Float_Matrix;
                              N_Informative, N_Redundant,
                              N_Repeated : Natural) is
      Offset  : constant Natural := N_Informative + N_Redundant;
      F_Num   : constant Float := Float (Offset - 1);
      Indices : Integer_Array (1 .. N_Repeated);
      X1      : Real_Float_Matrix (X'Range, Indices'Range);
   begin
      for item in Indices'Range loop
         Indices (item) := Integer (F_Num *  Maths.Random_Float + 0.5);
      end loop;

      for row in X1'Range loop
         for col in X1'Range (2) loop
            X1 (row, col) := X (row, col);
         end loop;
      end loop;

      for row in X1'Range loop
         for col in X1'Range (2) loop
            X (row , col + Offset - 1) := X1 (row, col);
         end loop;
      end loop;

   end Repeat_Features;

   --  ------------------------------------------------------------------------

   procedure Shuffle_Data (X          : in out Real_Float_Matrix;
                           Y          : in out Integer_Array;
                           N_Features : Positive) is
      Row_Indicies     : Integer_Array (X'Range);
      Feature_Indicies : Integer_Array (1 .. N_Features);
      P_X              : Real_Float_Matrix (X'Range, X'Range (2));
      P_Y              : Integer_Array (Y'Range);
   begin
      for index in Row_Indicies'Range loop
         Row_Indicies (index) := index;
      end loop;
      Utilities.Permute (Row_Indicies);

      for index in Feature_Indicies'Range loop
         Feature_Indicies (index) := index;
      end loop;
      Utilities.Permute (Feature_Indicies);

      for row in X'Range loop
         P_Y (row) := Y (Row_Indicies (row));
         for col in X'Range (2) loop
            P_X (row, col) := X (Row_Indicies (row), Feature_Indicies (col));
         end loop;
      end loop;

      X := P_X;
      Y := P_Y;

   end Shuffle_Data;

   --  ------------------------------------------------------------------------

end Samples_Generator;
