
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Validation is
   --  Derived from python3.7/site-packages/sklearn/utils/Validation.py

   Package Attribute_Package is new  Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);
   type Attribute_List is new Attribute_Package.List with null Record;

   type Attributes_Type is (Shape, Fit, None);
   type Attributes_Names is array (Integer range <>) of Unbounded_String;
   type Estimator_Attributes is array (Integer range <>) of Unbounded_String;
   type State is (None, All_States, Any);
   Empty_String : constant Unbounded_String := To_Unbounded_String ("");

   Not_Fitted_Error : Exception;

   --  Check_Is_Fitted checks if the estimator is fitted by verifying the
   --  presence of fitted attributes (ending with a trailing underscore).
   --  Otherwise raises a Not_Fitted_Error with the given message.
   --  This utility is meant to be used internally by estimators themselves.
   procedure Check_Is_Fitted
     (Estimator  : Attribute_List;
      Attributes : Attribute_Package.List := Attribute_Package.Empty_List;
      Msg        : Unbounded_String := Empty_String;
      All_Or_Any : State := All_States);

end Validation;
