
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Encode_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Stochastic_Optimizers;

package Neural_Printing is

   Print_Error : Exception;

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
   procedure Print_Float_Lists_2D (Name  : String;
                                   Data  : NL_Types.Float_List_2D;
                                   Start : Positive := 1;
                                   Last  : Positive := 10);
   procedure Print_Float_Lists_3D (Name : String;
                                   Data : NL_Types.Float_List_3D);
   procedure Print_Float_Matrix
      (Name  : String; aMatrix : Real_Float_Matrix;
       Start : Integer := 1; Finish : Integer := 0;
       Col_Start : Integer := 1; Col_Finish : Integer := 0);
   procedure Print_Float_Matrix_Formated
     (Name  : String; aMatrix : Real_Float_Matrix; Places : Natural;
      Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Matrix (Name  : String; aMatrix : Integer_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix);
   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array);
   procedure Print_Natural_List (Name : String; theList : NL_Types.Natural_List);
   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : NL_Types.Natural_Lists_2D);
   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10);
   procedure Print_Real_Vector_List (Name        : String;
                                     Vector_List : Real_Vector_List);
   procedure Print_Slice (Name : String; theSlice : NL_Types.Slice_Record);
   procedure Print_Slices (Name  : String; theList : NL_Types.Slices_List;
                           Start : Positive := 1; Last : Positive := 10);
   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set);
end ;
