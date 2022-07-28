--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Samples_Generator is

   type Return_Indicator_Type is (RI_Dense);

   type Classification_Data
     (N_Samples, N_Features, N_Classes  : Positive;
      Distributions                     : Boolean)
   is record
      X : Real_Float_Matrix (1 .. N_Samples, 1 .. N_Features);
      Y : Binary_Matrix (1 .. N_Samples, 1 .. N_Classes);
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

   function Make_Classification
     (N_Samples            : Positive := 100;
      N_Features           : Positive := 20;
      N_Informative        : Positive := 20;
      N_Redundant          : Positive := 20;
      N_Repeated           : Natural := 0;
      N_Classes            : Positive := 2;
      N_Clusters_per_Class : Positive := 2;
      Weights              : NL_Types.Float_List;
      Flip_Y               : Float := 0.01;
      Class_Sep            : Float := 1.0;
      Hypercube            : Boolean := True;
      Shift                : Float := 0.0;
      Scale                : Float := 1.0;
      Shuffle              : Boolean := True)
      return Classification_Data;

   function Make_Multilabel_Classification
     (N_Samples            : Positive := 100;
      N_Features           : Positive := 20;
      N_Classes            : Positive := 5;
      N_Labels             : Positive := 2;
      Expected_Length      : Positive := 50;
      Allow_Unlabeled      : Boolean := True;
      --         Sparse : Boolean := False;
      --         Return_Indicator : Return_Indicator_Type := RI_Dense;
      Return_Distributions : Boolean := False)
      return Classification_Data;

end Samples_Generator;
