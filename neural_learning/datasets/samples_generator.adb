--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with Maths;

package body Samples_Generator is

    function Make_Multilabel_Classification
      (N_Samples : Positive := 100; N_Features : Positive := 20;
       N_Classes : Positive := 5; N_labels : Positive := 2;
       Length : Positive := 50; Allow_Unlabeled : Boolean := True;
       Sparse : Boolean := False;
       Return_Indicator : Return_Indicator_Type := RI_Dense;
       Return_Distributions : Boolean := False)
       return Multilabel_Classification is

        procedure Sample_Example is
            Y_Size : Positive := N_Classes + 1;
        begin
            while not Allow_Unlabeled loop
                null;
            end loop;
        end Sample_Example;

        P_C            : Float_Array (1 .. N_Classes);
        Cum_P_C        : Float_Array (1 .. N_Classes);
        P_C_Sum        : Float := 0.0;
        P_W_C          : Real_Float_Matrix (1 .. N_Features, 1 .. N_Classes);
        Classification : Multilabel_Classification
          (N_Samples, N_Features, N_Classes);
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

        return Classification;

    end Make_Multilabel_Classification;

end Samples_Generator;
