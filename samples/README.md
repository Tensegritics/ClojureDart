This repo aims at training yourself and others by practicing/reading Flutter the ClojureDart way.
You will find listed below in the `Sample` section all of the current samples, and what do they cover.

## Run a sample

```sh
clj -M:cljd flutter
```
A native application will be created. If you want to test it on mobile, please follow [those instructions.](https://github.com/Tensegritics/ClojureDart/blob/main/doc/flutter-quick-start.md#7-start-a-simulator)

## Samples

### [Animated container](./animated_container)

- state with `:state`

- Particular widgets
    - AnimatedContainer
    - BoxDecoration
    - FloatingActionButton

### [Counter](./counter)
- state with `:state`
- Widget.of(context) with `:inherit`
- nested widgets

- Particular widgets
    - Column
    - FloatingActionButton
    - Theme / ThemeData


### [DataTable](./datatable)
- stateless

- Particular widgets
    - SingleChildScrollView
    - DataTable


### [Drawer](./drawer)
- stateless
- Navigator in Scaffold widget with `:inherit`

- Particular widgets
    - Navigator
    - ListView
    - ListTile


### [Fab](./fab)
- BuildContext with `:context`
- key with `:key`
- state with `:state`
- Theme.of(context) with `:inherit`
- `:ticker`
- dispose/init ressources with `:with`

- Particular widgets
    - Theme / ThemeData
    - InkWell
    - AnimatedContainer
    - AnimatedOpacity
    - SizedBox
    - Icon
    - Stack
    - AnimatedBuilder
    - FadeTransition
    - BoxDecoration
    - Container
    - FloatingActionButton


### [Fade Widget](./fade_widget)
- state with `:state`

- Particular widgets
    - AnimatedOpacity
    - FloatingActionButton


### [Form - handle change TextField](./form_handle_change_textfield)
- state with `:state`
- `:controller`
- dispose/init ressources with `:with`

- Particular widgets
    - TextField


### [Form - Retrieve Input](./form_retrieve_input)
- `:controller`
- dispose/init ressources with `:with`

- Particular widgets
    - AlertDialog
    - FloatingActionButton

### [Form - with validation](./form_validate)
- ScaffoldMessenger.of(context) with `:get`
- `cljd.string/blank?`
- GlobalKey\<FormState\> with `#/(m/GlobalKey m/FormState)`

- Particular widgets
    - TextFormField
    - ScaffoldMessenger
    - SnackBar


### [Gesture Detector](./gesture_detector)
- state with `:state`
- anonymous class extention with `reify :extends`

- Particular widgets
    - CustomPaint
    - CustomPainter
    - Canvas
    - GestureDetector
    - Offset
    - Paint

### [GridList](./gridlist)
- stateless
- Particular widgets
    - GridView

### [Hero animations](./hero_animations)
- Navigator in Scaffold widgets with `:inherit`

- Particular widgets
    - Navigator
    - GestureDetector
    - Hero

### [Navigation](./navigation)
- Navigator in Scaffold widgets with `:inherit`

- Particular widgets
    - Navigator
    - ElevatedButton

### [Physics simulation](./physics_simulation)
- BuildContext with `:context`
- key with `:key`
- state with `:state`
- MediaQuery.of(context) with `:inherit`
- `:ticker`
- dispose/init ressources with `:with`

- Particular widgets
    - GestureDetector
    - SpringDetection
    - SpringSimulation
    - Align
    - Card

### [Snackbar](./snackbar)
- ScaffoldMessenger.of(context) with `:inherit`

- Particular widgets
    - ElevatedButton
    - SnackBar
    - SnackBarAction

### [Tabs](./tabs)
- stateless

- Particular widgets
    - DefaultTabController
    - TabBar
    - TabBarView

### [Two counters](./twocounters)
- closure with `:bind`
- Theme.of(context) with `:inherit`

- Particular widgets
    - Center
    - Row
    - ElevatedButton
