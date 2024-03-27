#pragma once
#include <type_traits>
#include <memory>

// Member object pointer concept

template<auto T>
concept IsMemberObjectPointer = std::is_member_object_pointer_v<decltype(T)>;

// Get class and value types from member object pointer

template <class T>
struct _member_ptr_types;

template <class C, class T>
struct _member_ptr_types<T C::*>
{
    using Class = C;
    using Value = T;
};

template <auto P>
requires IsMemberObjectPointer<P>
struct member_ptr_types : _member_ptr_types<std::remove_cvref_t<decltype(P)>> {};

template <auto P>
using member_ptr_class_t = member_ptr_types<P>::Class;

template <auto P>
using member_ptr_value_t = member_ptr_types<P>::Value;

// Type identification concepts

template<typename T>
concept IsSharedPtr = std::is_same_v<T, std::shared_ptr<typename T::element_type>>;

template<typename T>
concept IsVector = std::is_same_v<T, std::vector<typename T::value_type>>;

template <typename T, typename... U>
concept IsAnyOf = (std::same_as<T, U> || ...);
