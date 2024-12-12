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
            $($vis)* exported_types: $t<KeyExported>,
            $($vis)* annotations: $t<KeyAnnotation>,
            $($vis)* mros: $t<KeyClassMetadata>,
            $($vis)* legacy_tparams: $t<KeyLegacyTypeParam>,
        }

        impl $crate::alt::table::TableKeyed<Key> for $name {
            type Value = $t<Key>;
            fn get(&self) -> &Self::Value { &self.types }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.types }
        }

        impl $crate::alt::table::TableKeyed<KeyExported> for $name {
            type Value = $t<KeyExported>;
            fn get(&self) -> &Self::Value { &self.exported_types }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.exported_types }
        }

        impl $crate::alt::table::TableKeyed<KeyAnnotation> for $name {
            type Value = $t<KeyAnnotation>;
            fn get(&self) -> &Self::Value { &self.annotations }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.annotations }
        }

        impl $crate::alt::table::TableKeyed<KeyClassMetadata> for $name {
            type Value = $t<KeyClassMetadata>;
            fn get(&self) -> &Self::Value { &self.mros }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.mros }
        }

        impl $crate::alt::table::TableKeyed<KeyLegacyTypeParam> for $name {
            type Value = $t<KeyLegacyTypeParam>;
            fn get(&self) -> &Self::Value { &self.legacy_tparams }
            fn get_mut(&mut self) -> &mut Self::Value { &mut self.legacy_tparams }
        }

        impl $name {
            #[allow(dead_code)]
            fn get<K>(&self) -> &<Self as $crate::alt::table::TableKeyed<K>>::Value
            where
                Self: $crate::alt::table::TableKeyed<K>,
            {
                $crate::alt::table::TableKeyed::<K>::get(self)
            }

            #[allow(dead_code)]
            fn get_mut<K>(&mut self) -> &mut <Self as $crate::alt::table::TableKeyed<K>>::Value
            where
                Self: $crate::alt::table::TableKeyed<K>,
            {
                $crate::alt::table::TableKeyed::<K>::get_mut(self)
            }
        }
    };
}

#[macro_export]
macro_rules! table_for_each(
    ($e:expr, $f:expr) => {
        $f(&($e).types);
        $f(&($e).exported_types);
        $f(&($e).annotations);
        $f(&($e).mros);
        $f(&($e).legacy_tparams);
    };
);

#[macro_export]
macro_rules! table_mut_for_each(
    ($e:expr, $f:expr) => {
        $f(&mut ($e).types);
        $f(&mut ($e).exported_types);
        $f(&mut ($e).annotations);
        $f(&mut ($e).mros);
        $f(&mut ($e).legacy_tparams);
    };
);

#[macro_export]
macro_rules! table_try_for_each(
    ($e:expr, $f:expr) => {
        $f(&($e).types)?;
        $f(&($e).exported_types)?;
        $f(&($e).annotations)?;
        $f(&($e).mros)?;
        $f(&($e).legacy_tparams)?;
    };
);
