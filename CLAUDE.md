# Cellium Project Guide

## Project Overview
**Cellium** is a terminal UI framework written in Erlang, inspired by The Elm Architecture (TEA). It provides a structured approach to building interactive terminal applications with a declarative widget system, layout management, and event handling.

## Architecture

### Core Concepts
- **TEA-inspired**: Model-View-Update architecture pattern
- **Widget-based UI**: Declarative components for building terminal interfaces
- **Layout Engine**: Automatic layout calculation with horizontal/vertical containers
- **Event System**: Centralized event management and dispatch
- **Theming/CSS**: Styling system for widgets

### Key Components

#### Application Layer (`apps/cellium/src/`)
- `cellium_app.erl` - OTP application entry point
- `cellium_sup.erl` - Main supervisor keeping services running
- `view.erl` - gen_server handling rendering loop (1000ms tick interval)
- `cellium_state.erl` - Global state management
- `cellium_event_manager.erl` - Event handling and dispatch
- `app_event_manager.erl` - Application-level event coordination

#### Layout & Rendering
- `layout_manager.erl` - Calculates widget positions and sizes
- `layout.erl` - Tree manipulation (find, append, replace, delete, update operations)
- `widgets.erl` - Widget rendering dispatcher
- `css.erl` - Theming and styling system

#### Widget Types
- `container.erl` - Layout containers (horizontal/vertical orientation)
- `box.erl` - Simple colored box widget
- `button.erl` - Interactive button widget
- `text.erl` - Text display widget
- `text_input.erl` - Single-line text input
- `text_area.erl` - Multi-line text area
- `status_bar.erl` - Status bar widget
- `time.erl` - Time display widget

#### Supporting Modules
- `termbox2_nif.erl` / `termbox_dummy.erl` - Terminal rendering backend (switchable)
- `input_handler.erl` - Keyboard/input processing
- `update_model.erl` - Model update logic (TEA update function)
- `theme.erl` - Theme definitions
- `demo.erl` - Example layouts and usage patterns
- `debug_layout.erl` - Layout debugging utilities

### Configuration Files
- `include/cellium.hrl` - Common macros (termbox colors, attributes)
- `include/trace.hrl` - Debug tracing macros (TEST mode only)
- `rebar.config` - Build configuration and dependencies

## Dependencies (from rebar.config)
- `termbox2_nif` (2.0.0) - Terminal rendering library
- `maps_in` (0.2.0) - Map manipulation utilities
- `erlmcp` (git) - MCP integration (from erlsci/erlmcp)

## Development Workflow

### Building
```bash
rebar3 compile
```

### Running
```bash
rebar3 shell
demo:simple().  % Start demo application
```

### Testing
- Tests located in `apps/cellium/test/`
- EUnit tests only (as per project conventions)
- Run with: `rebar3 eunit`

## Data Structures

### Widget/Container Map Structure
```erlang
#{
    type => container | widget,
    id => atom(),

    %% Container-specific
    orientation => horizontal | vertical,
    children => [widget()],

    %% Widget-specific
    widget_type => box | button | text | time | etc,

    %% Common layout properties
    x => integer(),
    y => integer(),
    width => integer(),
    height => integer(),
    size => integer(),      % Fixed size
    expand => boolean(),    % Take available space

    %% Styling
    color => integer(),
    class => atom()
}
```

## Rendering Flow
1. **Model** - Application state stored in `cellium_state`
2. **View tick** (1000ms) - `view.erl` gen_server receives tick
3. **Layout calculation** - `layout_manager:calculate_layout/3` computes positions
4. **Theming** - `css:style/1` applies theme/styles
5. **Render** - `widgets:render/1` dispatches to widget modules
6. **Present** - `termbox2_nif:tb_present/0` displays to terminal

## Event Flow
1. Input event → `input_handler`
2. Event → `cellium_event_manager`
3. Update → `update_model` (TEA update function)
4. New model → `cellium_state`
5. Next render tick picks up changes

## Current Status
- **Active**: Core layout engine and widget system functional
- **On Hold**: Per project instructions - "All erlang projects" on hold
- **Last Commits**:
  - Adding widgets and tests
  - Reorganized to Elm application structure
  - Layout engine rewrite
  - Input handling

## File Locations
- Source: `apps/cellium/src/`
- Tests: `apps/cellium/test/`
- Config: `config/`
- Headers: `include/`

## Notes
- Termbox backend is switchable via `cellium.hrl` define
- Debug tracing available with TEST flag (`-define(TEST, true)`)
- Layout system supports nested containers with expand/fixed sizing
- Widget tree is immutable; updates create new tree structures

---
*Project follows standard rebar3 OTP application structure*
