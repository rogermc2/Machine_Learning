
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classifer;
with Tree;
with ML_Types;

package body Classifier_Tests is
    use Classifier_Types;
    use ML_Types;

    --     type Clf_Criterions is (Gini, Entropy);
    --     type Reg_Criterions is (Squared_Error, Absolute_Error,
    --                             Friedman_Mse, Poisson);
    --     type Clf_Trees is (Decision_Tree_Classifier, Extra_Tree_Classifier);
    --     type Reg_Trees is (Decision_Tree_Regressor, Extra_Tree_Regressor);

    X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                    ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
    Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
    --     T_Array     : constant Multi_Value_Array (1 .. 3, 1 .. 2) :=
    --                     ((-1, -1), (2, 2), (3, 2));
    True_Result : constant Integer_Array (1 .. 3) := (-1, 1, 1);

    --  -------------------------------------------------------------------------

    procedure Test_Classification_Toy  is
        use Classifier_Utilities;
        use Decision_Tree_Classifer;

        Expected    : List_Of_Value_Data_Lists;
        theTree     : Classifier (Tree.Integer_Type, Tree.Integer_Type,
                                  Tree.Integer_Type);
        X           : constant List_Of_Value_Data_Lists :=
                        To_Multi_Value_List (X_Array);
        Y           : List_Of_Value_Data_Lists;
        Num_Samples : constant Natural := Natural (X.Element (1).Length);
        Classes     : Value_Data_List;
        Weights     : Weight_List;
    begin
        Put_Line ("Classification_Tests.Test_Classification_Toy:");
        if Num_Samples = 0 then
            raise Classifier_Test_Error with
              "Classification_Tests.Test_Classification_Toy called with empty X vector.";
        end if;

        Init (theTree, Random_State => 0);
        Y := To_Integer_Value_List (Y_Array);
        Expected := To_Integer_Value_List (True_Result);
        Fit (theTree, X, Y, Weights);
        Put_Line ("Classification_Tests.Test_Classification_Toy Tree size: " &
                    Integer'Image
                    (Integer (theTree.Attributes.Decision_Tree.Nodes.Node_Count)));
        Print_Float_List ("Classification_Tests.Test_Classification_Toy weights",
                          Weights);
        Classes := Predict (theTree, X);
        Print_Value_List ("Classification_Tests.Test_Classification_Toy Classes",
                          Classes);

    end Test_Classification_Toy;

    --  -------------------------------------------------------------------------

end Classifier_Tests;
