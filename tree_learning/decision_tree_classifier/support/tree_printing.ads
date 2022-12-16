
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Build_Utils;
with Criterion;
with Base_Decision_Tree;
with Encode_Utils;
with ML_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with NL_Types;
with Node_Splitter;
with Tree;
with Weights;

package Tree_Printing is

    Print_Error : Exception;

    procedure Print_Boolean_Matrix (Name    : String;
                                    aMatrix : Boolean_Matrix);
    procedure Print_Boolean_List (Name : String; aList : NL_Types.Boolean_List;
                                  Start  : Integer := 1; Finish : Integer := 0);
    procedure Print_Criterion (Name : String;
                               Data : Criterion.Criterion_Class);
    procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
    procedure Print_Float_Array (Name   : String; anArray : Float_Array;
                                 Start  : Integer := 1;
                                 Finish : Integer := 0);
    procedure Print_Float_List (Name  : String; theList : NL_Types.Float_List);
--      procedure Print_Indefinite_List (Name   : String;
--                                      theList : ML_Types.Indef_String_List);
--      procedure Print_Integer_List (Name : String;
--                                    theList : ML_Types.Integer_List);
--      procedure Print_Integer_List (Name : String;
--                                    theList : ML_Types.Integer_DL_List);
    procedure Print_Integer_List (Name : String; theList : ML_Types.Integer_List);
    procedure Print_Natural_Lists_2D (Name : String;
                                      Data : NL_Types.Natural_Lists_2D);
    procedure Print_Float_Lists_2D (Name : String; Data : NL_Types.Float_List_2D);
    procedure Print_Multi_Value_Array (Name    : String;
                                       anArray : Multi_Value_Array);
    procedure Print_Value_Lists_2D
      (Name : String; Multi_List : Tree.Values_List_2D);
    procedure Print_Value_Lists_3D
      (Name : String; theList : Tree.Values_List_3D);
    procedure Print_Natural_List (Name : String; theList : NL_Types.Natural_List);
    procedure Print_Node (Message : String; Node : Tree.Tree_Node);
    procedure Print_Node (Message : String; Node : Tree.Tree_Cursor);
    procedure Print_Node_Cursor_Array (Name    : String;
                                       Cursors : Tree.Leaf_Cursor_Array);
    procedure Print_Node_Cursor_List (Name    : String;
                                      Cursors : Tree.Tree_Cursor_List);
    procedure Print_Split_Record (Name : String;
                                  Data : Node_Splitter.Split_Record);
    procedure Print_Stack_Record (Name : String;
                                  Data : Build_Utils.Stack_Record);
--      procedure Print_Strings (Name : String; theList : ML_Types.String_List);
--      procedure Print_Strings (Name    : String;
--                               theList : ML_Types.Indef_String_List);
    procedure Print_Tree (Name : String; aTree : Base_Decision_Tree.Classifier);
    procedure Print_Tree (Name : String; aTree : Tree.Tree_Class);
    procedure Print_Unbounded_List (Name    : String;
                                    theList : ML_Types.Unbounded_List);
    procedure Print_Unbounded_Set (Name   : String;
                                   theSet : Encode_Utils.UB_String_Sets.Set);
    procedure Print_Value_List (Name : String; theList : Tree.Values_List);
    procedure Print_Value_Data_List (Name    : String;
                                     theList : ML_Types.Value_Data_List);
--      procedure Print_Value_Data_Lists_2D (Name      : String;
--                                           theList   : ML_Types.Value_Data_Lists_2D;
--                                           Num_Items : Positive := 1000);
--      procedure Print_Value_Data_Lists_3D (Name    : String;
--                                           theList : ML_Types.Value_Data_Lists_3D);
--      procedure Print_Value_Record (Name : String; Value : ML_Types.Value_Record);
    procedure Print_Weights (Name : String; Data : Weights.Weight_List);
    procedure Print_Weight_Lists_2D (Name : String;
                                     Data : Weights.Weight_Lists_2D);
    procedure Print_Weight_Lists_3D (Name : String;
                                     Data : Weights.Weight_Lists_3D);

end Tree_Printing;
