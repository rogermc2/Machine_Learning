
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with Estimator;
with ML_Types;
with Base_Decision_Tree;
with Node_Splitter;
with Tree;
with Weights;

package Printing is

    Print_Error : Exception;

    procedure Print_Boolean_Matrix (Name    : String;
                                    aMatrix : Estimator.Boolean_Matrix);
    procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
    procedure Print_Float_Array (Name : String; anArray : Float_Array;
                                 Start, Finish : Integer);
    procedure Print_Float_List (Name  : String; theList : Float_List);
    procedure Print_Integer_List (Name : String; theList : Integer_List);
    procedure Print_Natural_Lists_2D (Name : String;
                                      Data : Natural_Lists_2D);
    procedure Print_Float_Lists_2D (Name : String; Data : Float_List_2D);
    procedure Print_Multi_Value_Array (Name : String;
                                       anArray : Multi_Value_Array);
    procedure Print_Value_Lists_2D
      (Name : String; Multi_List : Tree.Values_List_2D);
    procedure Print_Value_Lists_3D
      (Name : String; theList : Tree.Values_List_3D);
    procedure Print_Natural_List (Name : String; theList : Natural_List);
    procedure Print_Node (Message : String; Node : Tree.Tree_Node);
    procedure Print_Node_Cursor_Array (Name    : String;
                                       Cursors : Tree.Leaf_Cursor_Array);
    procedure Print_Node_Cursor_List (Name    : String;
                                      Cursors : Tree.Tree_Cursor_List);
    procedure Print_Split_Record (Name : String;
                                  Data : Node_Splitter.Split_Record);
    procedure Print_Tree (Name : String; aTree : Base_Decision_Tree.Classifier);
    procedure Print_Tree (Name  : String; aTree : Tree.Tree_Class);
    procedure Print_Value_List (Name : String; theList : Tree.Values_List);
    procedure Print_Value_Data_List (Name    : String;
                                     theList : ML_Types.Value_Data_List);
    procedure Print_Value_Data_Lists_2D (Name    : String;
                                        theList : ML_Types.Value_Data_Lists_2D);
    procedure Print_Value_Data_Lists_3D (Name    : String;
                                         theList : ML_Types.Value_Data_Lists_3D);
    procedure Print_Weights (Name : String; Data : Weights.Weight_List);
    procedure Print_Weights_Lists_2D (Name : String;
                                      Data : Weights.Weight_Lists_2D);
   procedure Print_Weight_Lists_3D (Name : String;
                                    Data : Weights.Weight_Lists_3D);

end Printing;
