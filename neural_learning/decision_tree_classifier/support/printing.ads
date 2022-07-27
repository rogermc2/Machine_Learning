
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;
with Encode_Utils;
with Export_Types;
with Stochastic_Optimizers;

package Printing is

   Print_Error : Exception;

   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix;
                                  Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Boolean_Array
     (Name : String; anArray : NL_Arrays_And_Matrices.Boolean_Array);
   procedure Print_Boolean_Matrix (Name  : String; aMatrix : Boolean_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Bounds (Name : String; Data : Export_Types.Bounds_List);
   procedure Print_Colours_List (Name    : String;
                                 Colours : Export_Types.Colours_List);
   procedure Print_Integer_Colours_List
     (Name : String; Colours : Export_Types.Integer_Colours_List);
   procedure Print_Integer_Array
     (Name  : String; anArray : NL_Arrays_And_Matrices.Integer_Array;
      Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Export_Map (Name : String; aMap : Export_Types.Export_Map);
   procedure Print_Float_Array (Name   : String; anArray : Real_Float_Vector;
                                Start  : Integer := 1;
                                Finish : Integer := 0);
   procedure Print_Float_List (Name    : String; theList : Float_List);
   procedure Print_Float_Matrix (Name  : String; aMatrix : Real_Float_Matrix;
                                 Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Float_Matrix_Formated
     (Name  : String; aMatrix : Real_Float_Matrix; Places : Natural;
      Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Indefinite_List (Name    : String;
                                    theList : Indef_String_List);
   procedure Print_Integer_List (Name  : String; theList : Integer_List;
                                 Start : Positive := 1; Last : Positive := 10);
   procedure Print_Integer_List (Name    : String; theList : Integer_DL_List);
   procedure Print_Integer_List_Array
     (Name    : String; theArray : Integer_List_Array;
      Start   : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Matrix (Name  : String; aMatrix : Integer_Matrix;
                                   Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_Set (Name   : String;
                                theSet : Encode_Utils.Int_Sets.Set);
   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : Natural_Lists_2D);
   procedure Print_Float_Lists_2D (Name  : String; Data : Float_List_2D;
                                   Start : Positive := 1;
                                   Last  : Positive := 10);
   procedure Print_Float_Lists_3D (Name : String; Data : Float_List_3D);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix);
   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array);
   procedure Print_Natural_List (Name : String; theList : Natural_List);
   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10);
   procedure Print_RGB_Array (Name : String; anArray : Export_Types.RGB_Array);
   procedure Print_Slice (Name : String; theSlice : Slice_Record);
   procedure Print_Slices (Name  : String; theList : Slices_List;
                           Start : Positive := 1; Last : Positive := 10);
   procedure Print_Strings (Name : String; theList : String_List);
   procedure Print_Strings (Name : String; theList : String_Vector);
   procedure Print_Strings (Name    : String;
                            theList : Indef_String_List);
   procedure Print_Unbounded_List (Name    : String;
                                   theList : Unbounded_List);
   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set);
   procedure Print_Value_Data_List (Name    : String;
                                    theList : Value_Data_List);
   procedure Print_Value_Data_Lists_2D (Name      : String;
                                        theList   : Value_Data_Lists_2D;
                                        Num_Items : Positive := 1000);
   procedure Print_Value_Data_Lists_3D (Name    : String;
                                        theList : Value_Data_Lists_3D);
   procedure Print_Value_Record (Name : String; Value : Value_Record);

end Printing;
