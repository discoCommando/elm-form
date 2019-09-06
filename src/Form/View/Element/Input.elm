module Form.View.Element.Input exposing (..)


import Form.View2
import Form.Transaction as Transaction exposing (Transaction) 
import Form.FieldState as FieldState
import Form.Get as Get
import Form.Field as Field
import Element
import Element.Input
import Form.View.Element exposing (Element(..), Attribute, Label(..), Placeholder(..), LabelLocation(..), TextInputType(..))


-- TODO in the future add a custom functions for each 
text : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes: List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field) }) -> Element error field (Transaction field) 
text field cont = 
    EL_TEXT TI_text field (\errorState -> 
        let
            { attributes, placeholder, label } = cont errorState   
        in
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        , onChange = Transaction.setString field
        })


spellChecked : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field) }) -> Element error field (Transaction field) 
spellChecked field cont = 
    EL_TEXT TI_spellChecked field (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        , onChange = Transaction.setString field
        })

search : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field) }) -> Element error field (Transaction field)
search field cont = 
    EL_TEXT TI_search field (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        , onChange = Transaction.setString field
        })

newPassword : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field), show: Bool }) -> Element error field (Transaction field)
newPassword field cont = 
    EL_TEXT TI_newPassword field (\errorState ->
        let 
            { attributes, placeholder, label, show } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = show
        , spellcheck = False
        , onChange = Transaction.setString field
        })

currentPassword : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field), show: Bool }) -> Element error field (Transaction field)
currentPassword field cont = 
    EL_TEXT TI_currentPassword field (\errorState ->
        let 
            { attributes, placeholder, label, show } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = show
        , spellcheck = False
        , onChange = Transaction.setString field
        })

username : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field) }) -> Element error field (Transaction field)
username field cont = 
    EL_TEXT TI_username field (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        , onChange = Transaction.setString field
        })

email : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field) }) -> Element error field (Transaction field)
email field cont = 
    EL_TEXT TI_email field (\errorState ->
        let 
            { attributes, placeholder, label } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = False
        , onChange = Transaction.setString field
        })

multiline : (Field.Value String -> field) -> (FieldState.ErrorState error -> { attributes : List (Attribute error field (Transaction field)), placeholder : Maybe (Placeholder error field (Transaction field)), label: Label error field (Transaction field), spellcheck: Bool }) -> Element error field (Transaction field)
multiline field cont = 
    EL_TEXT TI_multiline field (\errorState ->
        let 
            { attributes, placeholder, label, spellcheck } = cont errorState
        in 
        { attributes = attributes
        , placeholder = placeholder
        , label = label 
        , show = False
        , spellcheck = spellcheck
        , onChange = Transaction.setString field
        })
     

labelRight : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelRight =
    L_Label LL_labelRight

labelLeft : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelLeft =
    L_Label LL_labelLeft

labelAbove : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelAbove =
    L_Label LL_labelAbove

labelBelow : List (Attribute error field msg) -> Element error field msg -> Label error field msg
labelBelow =
    L_Label LL_labelBelow

labelHidden : String -> Label error field msg 
labelHidden =
    L_Hidden
