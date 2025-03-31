# TypeScript Migration Guide

This document outlines the migration of the Pyre2 website from JavaScript/Flow
to TypeScript.

## What Was Done

1. **TypeScript Configuration Setup**:

   - Created `tsconfig.json` with appropriate compiler options
   - Updated `package.json` to add TypeScript and type definitions
     (@types/react, @types/node, etc.)
   - Updated `babel.config.js` to use @babel/preset-typescript instead of
     @babel/preset-flow

2. **Type Definitions**:

   - Created type definitions for StyleX in `src/types/stylex.d.ts`
   - Created type definitions for CSS modules in `src/types/css-modules.d.ts`
   - Created type definitions for JSON modules in `src/types/json-modules.d.ts`

3. **Converted Components**:

   - Landing page components:

     - `PerformanceComparisonTypes.js` → `PerformanceComparisonTypes.ts`
     - `PerformanceComparisonChartSection.js` →
       `PerformanceComparisonChartSection.tsx`
     - `PerformanceComparisonButton.js` → `PerformanceComparisonButton.tsx`
     - `PerformanceComparisonChart.js` → `PerformanceComparisonChart.tsx`
     - `ProgressBar.js` → `ProgressBar.tsx`
     - `quoteCard.js` → `quoteCard.tsx`
     - `landingPageSection.js` → `landingPageSection.tsx`
     - `landingPageHeader.js` → `landingPageHeader.tsx`
     - `firefly.js` → `firefly.tsx`
     - `whyPyrefly.js` → `whyPyrefly.tsx`
     - `whyPyreflyGridItem.js` → `whyPyreflyGridItem.tsx`
     - `quotesGrid.js` → `quotesGrid.tsx`

   - Try Pyre2 components:

     - `TryPyre2.js` → `TryPyre2.tsx`
     - `TryPyre2Results.js` → `TryPyre2Results.tsx`
     - `pyre2_wasm.js` → `pyre2_wasm.ts`
     - `configured-monaco.js` → `configured-monaco.ts`

   - Pages:
     - `index.js` → `index.tsx`
     - `newLandingPage.js` → `newLandingPage.tsx`

## Type Improvements

1. **Component Props**:

   - Added interfaces for component props
   - Used proper TypeScript types for props

2. **Function Signatures**:

   - Added return types to functions
   - Added parameter types to functions

3. **StyleX Types**:

   - Created type definitions for StyleX to ensure type safety

4. **Flow to TypeScript Conversion**:
   - Converted Flow-specific syntax like `?$ReadOnlyArray<T>` to TypeScript
     equivalents like `ReadonlyArray<T> | null`
   - Changed component syntax from `component ComponentName(props)` to
     `function ComponentName(props): React.ReactElement`

## Next Steps

1. **Run TypeScript Compiler**:

   ```bash
   npx tsc --noEmit
   ```

   This will check for any type errors in the codebase.

2. **Remove Flow Configuration**:

   - Remove `flow-typed` directory
   - Remove Flow dependencies from `package.json`

3. **Update Imports**:

   - Update any imports that still reference `.js` files to reference `.tsx` or
     `.ts` files instead

4. **Test the Website**:
   ```bash
   yarn start
   ```
   This will start the development server and allow you to test the website.

## Alternative CSS-in-JS Libraries

If you encounter issues with StyleX in TypeScript, consider these alternatives:

1. **Emotion**:

   - Good TypeScript support
   - Similar API to StyleX
   - Installation: `yarn add @emotion/react @emotion/styled`

2. **Styled Components**:

   - Excellent TypeScript support
   - Component-based styling
   - Installation: `yarn add styled-components @types/styled-components`

3. **CSS Modules**:
   - Already being used in parts of the project
   - Good TypeScript support with the type definitions we've added
   - No additional dependencies needed
