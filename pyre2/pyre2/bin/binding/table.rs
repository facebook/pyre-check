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
            $($vis)* types: $t<Key>,
            $($vis)* exports: $t<KeyExport>,
            $($vis)* class_fields: $t<KeyClassField>,
            $($vis)* annotations: $t<KeyAnnotation>,
            $($vis)* mros: $t<KeyClassMetadata>,
            $($vis)* legacy_tparams: $t<KeyLegacyTypeParam>,
        }

        impl $crate::binding::table::TableKeyed<Key> for $name {
            type Value = $t<Key>;
            fn get(&self) -> &Self::Value { &self.types }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.types }
        }

        impl $crate::binding::table::TableKeyed<KeyExport> for $name {
            type Value = $t<KeyExport>;
            fn get(&self) -> &Self::Value { &self.exports }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.exports }
        }

        impl $crate::binding::table::TableKeyed<KeyClassField> for $name {
            type Value = $t<KeyClassField>;
            fn get(&self) -> &Self::Value { &self.class_fields }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.class_fields }
        }


        impl $crate::binding::table::TableKeyed<KeyAnnotation> for $name {
            type Value = $t<KeyAnnotation>;
            fn get(&self) -> &Self::Value { &self.annotations }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.annotations }
        }

        impl $crate::binding::table::TableKeyed<KeyClassMetadata> for $name {
            type Value = $t<KeyClassMetadata>;
            fn get(&self) -> &Self::Value { &self.mros }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.mros }
        }

        impl $crate::binding::table::TableKeyed<KeyLegacyTypeParam> for $name {
            type Value = $t<KeyLegacyTypeParam>;
            fn get(&self) -> &Self::Value { &self.legacy_tparams }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.legacy_tparams }
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
        $f(&($e).exports);
        $f(&($e).class_fields);
        $f(&($e).annotations);
        $f(&($e).mros);
        $f(&($e).legacy_tparams);
    };
);

#[macro_export]
macro_rules! table_mut_for_each(
    ($e:expr, $f:expr) => {
        $f(&mut ($e).types);
        $f(&mut ($e).exports);
        $f(&mut ($e).class_fields);
        $f(&mut ($e).annotations);
        $f(&mut ($e).mros);
        $f(&mut ($e).legacy_tparams);
    };
);

#[macro_export]
macro_rules! table_try_for_each(
    ($e:expr, $f:expr) => {
        $f(&($e).types)?;
        $f(&($e).exports)?;
        $f(&($e).class_fields)?;
        $f(&($e).annotations)?;
        $f(&($e).mros)?;
        $f(&($e).legacy_tparams)?;
    };
);
