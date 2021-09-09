
with Ada.Text_IO; use Ada.Text_IO;

with Decision_Tree_Classifer;
with tree;

package body Classifier_Tests is

   type Clf_Criterions is (Gini, Entropy);
   type Reg_Criterions is (Squared_Error, Absolute_Error,
                           Friedman_Mse, Poisson);
   type Clf_Trees is (Decision_Tree_Classifier, Extra_Tree_Classifier);
   type Reg_Trees is (Decision_Tree_Regressor, Extra_Tree_Regressor);

   --  -------------------------------------------------------------------------

   procedure Test_Classification_Toy  is
      use Decision_Tree_Classifer;
      aTree : Classifier (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
   begin
      Put_Line ("Classification_Tests.Test_Classification_Toy:");

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

   --  -------------------------------------------------------------------------

end Classifier_Tests;
