
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ML_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_U8_Types;
with NL_Types;

package Basic_Printing is

   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix;
                                  Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Boolean_List (Name  : String;
                                 aList : NL_Types.Boolean_List);
   procedure Print_Boolean_Array (Name  : String; anArray : Boolean_Array;
                                  Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Byte_Array (Name  : String; anArray : ML_U8_Types.Byte_Array;
                               Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_List (Name : String; theList : NL_Types.Float_List);
   procedure Print_Float_Lists_2D (Name  : String;
                                   Data  : NL_Types.Float_List_2D;
                                   Start : Positive := 1;
                                   Last  : Positive := 10);
   procedure Print_Float_Matrix
     (Name       : String; aMatrix : Real_Float_Matrix;
      Start      : Positive := 1; Finish : Natural := 0;
      Col_Start  : Positive := 1;
      Col_Finish : Natural := 0);
   procedure Print_Float_Matrix_List
     (Name  : String; aList : Real_Matrix_List;
      Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Vector (Name  : String; Vec : Real_Float_Vector;
                                 Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Float_Vector_As_Line
     (Name   : String; Vec : Real_Float_Vector;  Start : Positive := 1;
      Finish : Natural := 0);
   procedure Print_Float_Vector_List
     (Name       : String; aList : Float_Vector_List;
      List_Start : Positive := 1; List_Finish : Natural := 0;
      Start      : Positive := 1; Finish : Natural := 0);
   procedure Print_Integer_Array (Name  : String; anArray : Integer_Array;
                                  Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Integer_Array_List
     (Name       : String; aList : Integer_Array_List;
      List_Start : Positive := 1; List_Finish : Natural := 0;
      Start      : Positive := 1; Finish : Natural := 0);
   procedure Print_Integer_List
     (Name          : String; aList : ML_Types.Integer_List;
      Start, Finish : Natural := 0);
   procedure Print_Integer_List_2D
     (Name          : String; aList : ML_Types.Integer_List_2D;
      Start, Finish : Natural := 0);
   procedure Print_Integer_Map (Name : String;
                                aMap : ML_Types.Integer_Label_Map);
   procedure Print_Integer_Matrix (Name  : String; aMatrix : Integer_Matrix;
                                   Start : Natural := 0; Finish : Integer := 0);
   procedure Print_List_Dimensions (Name : String; aList : Real_Float_List);
   procedure Print_List_Dimensions (Name  : String;
                                    aList : Real_Float_List_2D);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Binary_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Integer_Matrix);
   procedure Print_Matrix_Dimensions (Name    : String;
                                      aMatrix : Real_Float_Matrix);
   procedure Print_Matrix_Dimensions
     (Name    : String; aMatrix : ML_U8_Types.Unsigned_8_Array_3D);
   procedure Print_Real_Float_List
     (Name  : String; aList : Real_Float_List;
      Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Real_Float_List_2D
     (Name  : String; aList : Real_Float_List_2D;
      Start : Positive := 1; Finish : Natural := 0);
   procedure Print_Strings (Name : String; theList : ML_Types.String_List);
   procedure Print_Strings (Name : String; theList : ML_Types.String_Vector);
   procedure Print_Strings (Name    : String;
                            theList : ML_Types.Indef_String_List);
   procedure Print_Unbound_Array (Name : String;
                                  UB   : Unbounded_String_Array);
   procedure Print_Unbound_List (Name : String; UB : ML_Types.Unbounded_List);

   procedure Print_String_Map (Name  : String; aMap : ML_Types.String_Map;
                               Start : Natural := 0; Finish : Natural := 0);
   procedure Print_Unbound_Matrix (Name : String;
                                   UB   : Unbounded_String_Matrix);

end Basic_Printing;
