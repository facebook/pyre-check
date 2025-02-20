/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Things that abstract over the binding types.

pub trait TableKeyed<K> {
    type Value;
    fn get(&self) -> &Self::Value;
    fn get_mut(&mut self) -> &mut Self::Value;
}

#[macro_export]
macro_rules! table {
    (#[$($derive:tt)*] pub struct $name:ident(pub $t:tt)) => {
        table!(@impl, [pub], #[$($derive)*] pub struct $name($t));
    };
    (#[$($derive:tt)*] pub struct $name:ident($t:tt)) => {
        table!(@impl, [], #[$($derive)*] pub struct $name($t));
    };
    (@impl, [$($vis:tt)*], #[$($derive:tt)*] pub struct $name:ident($t:tt)) => {
        #[$($derive)*]
        pub struct $name {
            $($vis)* types: $t<$crate::binding::binding::Key>,
            $($vis)* expectations: $t<$crate::binding::binding::KeyExpect>,
            $($vis)* exports: $t<$crate::binding::binding::KeyExport>,
            $($vis)* functions: $t<$crate::binding::binding::KeyFunction>,
            $($vis)* classes: $t<$crate::binding::binding::KeyClass>,
            $($vis)* class_fields: $t<$crate::binding::binding::KeyClassField>,
            $($vis)* class_synthesized_fields: $t<$crate::binding::binding::KeyClassSynthesizedFields>,
            $($vis)* annotations: $t<$crate::binding::binding::KeyAnnotation>,
            $($vis)* class_metadata: $t<$crate::binding::binding::KeyClassMetadata>,
            $($vis)* legacy_tparams: $t<$crate::binding::binding::KeyLegacyTypeParam>,
            $($vis)* yields: $t<$crate::binding::binding::KeyYield>,
            $($vis)* yield_froms: $t<$crate::binding::binding::KeyYieldFrom>,
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::Key> for $name {
            type Value = $t<$crate::binding::binding::Key>;
            fn get(&self) -> &Self::Value { &self.types }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.types }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyExpect> for $name {
            type Value = $t<$crate::binding::binding::KeyExpect>;
            fn get(&self) -> &Self::Value { &self.expectations }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.expectations }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyExport> for $name {
            type Value = $t<$crate::binding::binding::KeyExport>;
            fn get(&self) -> &Self::Value { &self.exports }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.exports }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyFunction> for $name {
            type Value = $t<$crate::binding::binding::KeyFunction>;
            fn get(&self) -> &Self::Value { &self.functions }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.functions }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyClass> for $name {
            type Value = $t<$crate::binding::binding::KeyClass>;
            fn get(&self) -> &Self::Value { &self.classes }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.classes }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyClassField> for $name {
            type Value = $t<$crate::binding::binding::KeyClassField>;
            fn get(&self) -> &Self::Value { &self.class_fields }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.class_fields }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyClassSynthesizedFields> for $name {
            type Value = $t<$crate::binding::binding::KeyClassSynthesizedFields>;
            fn get(&self) -> &Self::Value { &self.class_synthesized_fields }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.class_synthesized_fields }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyAnnotation> for $name {
            type Value = $t<$crate::binding::binding::KeyAnnotation>;
            fn get(&self) -> &Self::Value { &self.annotations }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.annotations }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyClassMetadata> for $name {
            type Value = $t<$crate::binding::binding::KeyClassMetadata>;
            fn get(&self) -> &Self::Value { &self.class_metadata }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.class_metadata }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyLegacyTypeParam> for $name {
            type Value = $t<$crate::binding::binding::KeyLegacyTypeParam>;
            fn get(&self) -> &Self::Value { &self.legacy_tparams }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.legacy_tparams }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyYield> for $name {
            type Value = $t<$crate::binding::binding::KeyYield>;
            fn get(&self) -> &Self::Value { &self.yields }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.yields }
        }

        impl $crate::binding::table::TableKeyed<$crate::binding::binding::KeyYieldFrom> for $name {
            type Value = $t<$crate::binding::binding::KeyYieldFrom>;
            fn get(&self) -> &Self::Value { &self.yield_froms }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.yield_froms }
        }

        impl $name {
            #[allow(dead_code)]
            fn get<K>(&self) -> &<Self as $crate::binding::table::TableKeyed<K>>::Value
            where
                Self: $crate::binding::table::TableKeyed<K>,
            {
                $crate::binding::table::TableKeyed::<K>::get(self)
            }

            #[allow(dead_code)]
            fn get_mut<K>(&mut self) -> &mut <Self as $crate::binding::table::TableKeyed<K>>::Value
            where
                Self: $crate::binding::table::TableKeyed<K>,
            {
                $crate::binding::table::TableKeyed::<K>::get_mut(self)
            }
        }
    };
}

#[macro_export]
macro_rules! table_for_each(
    ($e:expr, $f:expr) => {
        $f(&($e).types);
        $f(&($e).expectations);
        $f(&($e).exports);
        $f(&($e).functions);
        $f(&($e).classes);
        $f(&($e).class_fields);
        $f(&($e).class_synthesized_fields);
        $f(&($e).annotations);
        $f(&($e).class_metadata);
        $f(&($e).legacy_tparams);
        $f(&($e).yields);
        $f(&($e).yield_froms);
    };
);

#[macro_export]
macro_rules! table_mut_for_each(
    ($e:expr, $f:expr) => {
        $f(&mut ($e).types);
        $f(&mut ($e).expectations);
        $f(&mut ($e).exports);
        $f(&mut ($e).functions);
        $f(&mut ($e).classes);
        $f(&mut ($e).class_fields);
        $f(&mut ($e).class_synthesized_fields);
        $f(&mut ($e).annotations);
        $f(&mut ($e).class_metadata);
        $f(&mut ($e).legacy_tparams);
        $f(&mut ($e).yields);
        $f(&mut ($e).yield_froms);
    };
);

#[macro_export]
macro_rules! table_try_for_each(
    ($e:expr, $f:expr) => {
        $f(&($e).types)?;
        $f(&($e).expectations)?;
        $f(&($e).exports)?;
        $f(&($e).functions)?;
        $f(&($e).classes)?;
        $f(&($e).class_fields)?;
        $f(&($e).class_synthesized_fields)?;
        $f(&($e).annotations)?;
        $f(&($e).class_metadata)?;
        $f(&($e).legacy_tparams)?;
        $f(&($e).yields)?;
        $f(&($e).yield_froms)?;
    };
);
