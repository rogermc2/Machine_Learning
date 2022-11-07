
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Encode_Utils;
with Stochastic_Optimizers;

package Printing is

   Print_Error : Exception;

   procedure Print_Array_Of_Integer_Lists
     (Name    : String; theArray : ML_Types.Array_Of_Integer_Lists;
      Start   : Integer := 1; Finish : Integer := 0);
   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix;
                                  Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Boolean_Array
     (Name : String; anArray : Boolean_Array);
   procedure Print_Boolean_Matrix (Name  : String; aMatrix : Boolean_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Array
     (Name  : String; anArray : Integer_Array;
      Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Float_Array (Name   : String; anArray : Real_Float_Vector;
                                Start  : Integer := 1;
                                Finish : Integer := 0);
   procedure Print_Float_List (Name    : String; theList : NL_Types.Float_List);
   procedure Print_Float_Matrix
      (Name  : String; aMatrix : Real_Float_Matrix;
       Start : Integer := 1; Finish : Integer := 0;
       Col_Start : Integer := 1; Col_Finish : Integer := 0);
   procedure Print_Float_Matrix_Formated
     (Name  : String; aMatrix : Real_Float_Matrix; Places : Natural;
      Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Indefinite_List (Name    : String;
                                    theList : ML_Types.Indef_String_List);
   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_List;
                                 Start : Positive := 1; Last : Positive := 10);
   procedure Print_Integer_List (Name    : String;
                                 theList : ML_Types.Integer_DL_List);
   procedure Print_Integer_Matrix (Name  : String; aMatrix : Integer_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set);
   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : NL_Types.Natural_Lists_2D);
   procedure Print_Float_Lists_2D (Name  : String;
                                   Data  : NL_Types.Float_List_2D;
                                   Start : Positive := 1;
                                   Last  : Positive := 10);
   procedure Print_Float_Lists_3D (Name : String; Data : NL_Types.Float_List_3D);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix);
   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array);
   procedure Print_Natural_List (Name : String; theList : NL_Types.Natural_List);
   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10);
   procedure Print_Real_Vector_List (Name        : String;
                                     Vector_List : Real_Vector_List);
   procedure Print_Slice (Name : String; theSlice : NL_Types.Slice_Record);
   procedure Print_Slices (Name  : String; theList : NL_Types.Slices_List;
                           Start : Positive := 1; Last : Positive := 10);
   procedure Print_Strings (Name : String; theList : ML_Types.String_List);
   procedure Print_Strings (Name : String; theList : ML_Types.String_Vector);
   procedure Print_Strings (Name    : String;
                            theList : ML_Types.Indef_String_List);
   procedure Print_Unbounded_List (Name    : String;
                                   theList : ML_Types.Unbounded_List);
   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set);
   procedure Print_Value_Data_List (Name    : String;
                                    theList : ML_Types.Value_Data_List);
   procedure Print_Value_Data_Lists_2D (Name      : String;
                                        theList   : ML_Types.Value_Data_Lists_2D;
                                        Num_Items : Positive := 1000);
   procedure Print_Value_Data_Lists_3D (Name    : String;
                                        theList : ML_Types.Value_Data_Lists_3D);
   procedure Print_Value_Record (Name : String; Value : ML_Types.Value_Record);

end Printing;
