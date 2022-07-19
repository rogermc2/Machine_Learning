--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with Ada.Containers;

with Maths;

with Classifier_Utilities;
with NL_Types;

package body Samples_Generator is

    function Make_Multilabel_Classification
      (N_Samples : Positive := 100; N_Features : Positive := 20;
       N_Classes : Positive := 5; N_labels : Positive := 2;
       Length : Positive := 50; Allow_Unlabeled : Boolean := True;
       Sparse : Boolean := False;
       Return_Indicator : Return_Indicator_Type := RI_Dense;
       Return_Distributions : Boolean := False)
       return Multilabel_Classification is

        Cum_P_C_List    : NL_Types.Float_List;

        procedure Sample_Example is
            use Ada.Containers;
            use Maths;
            Y_Size    : Natural := N_Classes + 1;
            Num_Words : Natural := 0;
            Y         : NL_Types.Integer_List;
            Class     : Integer;
        begin
            --  pick a nonzero number of labels by rejection sampling
            while (not Allow_Unlabeled and Y_Size = 0) or Y_Size > N_Classes loop
                Y_Size := Poisson_Single (Float (N_labels));
            end loop;

            declare
                Prob : NL_Types.Float_List;
            begin
                for index in 1 .. Y_Size - Natural (Y.Length) loop
                    Prob.Append (abs (Maths.Random_Float));
                end loop;

                while Natural (Y.Length) /= Y_Size loop
                    Class := Classifier_Utilities.Search_Sorted_Integer_List
                      (Cum_P_C_List, Prob);
                end loop;

                --  pick a nonzero document length by rejection sampling
                while Num_Words = 0 loop
                    Num_Words := Poisson_Single (Float (N_labels));
                end loop;
            end;

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

        for index in Cum_P_C'Range loop
            Cum_P_C_List.Append (Cum_P_C (index));
        end loop;

        return Classification;

    end Make_Multilabel_Classification;

end Samples_Generator;
