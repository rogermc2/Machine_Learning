--  Based on scikit-learn/sklearn/datasets/samples_generator.py

with Ada.Containers.Vectors;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Samples_Generator is

   type Return_Indicator_Type is (RI_Dense);

   type Classification_Test_Data
     (N_Samples, N_Features, N_Classes  : Positive)
   is record
      X : Real_Float_Matrix (1 .. N_Samples, 1 .. N_Features);
      Y : Binary_Matrix (1 .. N_Samples, 1 .. N_Classes);
   end record;

   type Multilabel_Classification_Test_Data
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

   package Shift_Package is new Ada.Containers.Vectors (Positive, Float);
   subtype Shift_List is Shift_Package.Vector;
   subtype Scale_List is Shift_Package.Vector;

   use Shift_Package;
   Default_Shift_List : constant Shift_List := Empty_Vector & 0.0;
   Default_Scale_List : constant Scale_List := Empty_Vector & 1.0;

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
      return Classification_Test_Data;
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
       return Multilabel_Classification_Test_Data;

end Samples_Generator;
