--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Samples_Generator is

   type Return_Indicator_Type is (RI_Dense);

   type Multilabel_Classification
     (N_Samples, N_Features, N_Classes  : Positive;
      Distributions                     : Boolean)
   is record
      X : Real_Float_Matrix (1 .. N_Samples, 1 .. N_Features);
      Y : Integer_Matrix (1 .. N_Samples, 1 .. N_Classes);
      case Distributions is
         when True =>
            --  The probability p_c of each class being drawn
            Class_Probability         : Float_Array (1 .. N_Classes);
            --  The probability p_w_c of each feature being drawn given each class
            Feature_Class_Probability : Real_Float_Matrix
              (1 .. N_Features, 1 .. N_Classes);
         when False => null;
      end case;
   end record;

   function Make_Multilabel_Classification
     (N_Samples            : Positive := 100;
      N_Features           : Positive := 20;
      N_Classes            : Positive := 5;
      N_labels             : Positive := 2;
      Expected_Length      : Positive := 50;
      Allow_Unlabeled      : Boolean := True;
      --         Sparse : Boolean := False;
      --         Return_Indicator : Return_Indicator_Type := RI_Dense;
      Return_Distributions : Boolean := False)
   return Multilabel_Classification;

end Samples_Generator;
