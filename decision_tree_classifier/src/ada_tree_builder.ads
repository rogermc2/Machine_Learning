
with Classifier_Types;
with ML_Types;
with Node_Splitter;
with Tree;
with Weights;

package Ada_Tree_Builder is

    type Tree_Builder is record
        Splitter              : Node_Splitter.Splitter_Class;
        --  Minimum samples in an internal node
        Min_Samples_Split     : Natural := 0;
        Min_Samples_Leaf      : Natural := 0;
        Min_Weight_Leaf       : Float := 0.0;
        Max_Depth             : Natural := 0;
        Min_Impurity_Decrease : Float := 0.0;
    end record;

    Ada_Tree_Build_Error : Exception;

    procedure Build_Tree
      (theTree       : in out Tree.Tree_Class;
       X             : ML_Types.List_Of_Value_Data_Lists;
       Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
       Sample_Weight : Weights.Weight_List);

end Ada_Tree_Builder;
