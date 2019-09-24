#include <utility>

#include <Python.h>
#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "../cpp/processprofile.h"

using namespace std::string_view_literals;
namespace py = pybind11;

struct Convert {
    // Profile data types to python conversion
    // TODO: pybind11 converts all basic types -> add converters for custom types only

    template<typename T>
    static inline py::object to_py(const std::shared_ptr<T>& v)
    {
        return to_py(*v);
    }

    template<typename... Ts>
    static inline py::object to_py(const std::variant<Ts...>& v)
    {
        return std::visit([&](const auto& v_) -> py::object { return to_py(v_); }, v);
    }

    static inline py::object to_py(const Profile::Integer& v)
    {
        return py::int_(v);
    }

    static inline py::object to_py(const Profile::Float& v)
    {
        return py::float_(v);
    }

    static inline py::object to_py(const Profile::Boolean& v)
    {
        return py::bool_(v);
    }

    static inline py::object to_py(const Profile::String& v)
    {
        return py::str(v);
    }

    static inline py::object to_py(const Profile::Dict& v)
    {
        py::dict dct;
        for (const auto& kv: v) {
            dct[py::str(kv.first)] = to_py(kv.second);
        }
        return std::move(dct);
    }

    static inline py::object to_py(const Profile::List& v)
    {
        py::list list;
        for (const auto& item: v) { list.append(to_py(item)); }
        return std::move(list);
    }

    static inline py::object to_py(const Profile::Object& v)
    {
        py::dict dct;
        dct["_type_"] = py::str("object");
        dct["_class_"] = py::str(v.classname);
        if (!v.id.empty()) {
            dct["_id_"] = py::str(v.id);
        }
        if (!v.children.empty()) {
            py::list children;
            for (const auto& item: v.children) {
                children.append(to_py(item));
            }
            dct["_children_"] = children;
        }
        for (const Profile::Attribute& a: v.attributes) {
            dct[py::str(a.name)] = to_py(a.data);
        }
        return std::move(dct);
    }

    static inline py::object to_py(const Profile::RawExpression& expr)
    {
        py::dict dct;
        dct["_type_"] = py::str("expression");
        dct["value"] = py::str(expr.str);
        return std::move(dct);
    }

    static inline py::object to_py(const Profile::Reference& ref)
    {
        py::dict dct;
        dct["_type_"] = py::str("reference");
        dct["reference"] = py::str(static_cast<const std::string&>(ref));
        return std::move(dct);
    }
};


class PyProfile {
public:
    PyProfile(std::string src)
        : m_src(std::move(src))
        , m_profile(Profile::parse(m_src))
    { }

    py::object to_ast()
    {
        const auto& [profile, ids] = m_profile;
        return Convert::to_py(profile);
    }

    std::string to_json_ast(const bool eval_expressions)
    {
        const auto& [profile, ids] = m_profile;
        return Profile::to_json_ast(profile, eval_expressions);
    }

    py::dict values()
    {
        const auto& [profile, ids] = m_profile;
        py::dict dct;
        for (const auto& kv : ids.value_ids) {
            dct[py::str(kv.first)] = Convert::to_py(kv.second->data);
        }
        return dct;
    }

private:
    std::string m_src;
    std::tuple<Profile::ObjectPtr, Profile::IdCollection> m_profile;
};


PYBIND11_MODULE(profile_parser, m) {
    py::class_<PyProfile> cls(m, "Profile");
    cls.def(py::init<std::string>());
    cls.def("values", &PyProfile::values, "Profile values.");
    cls.def("to_ast", &PyProfile::to_ast, "Profile AST representation.");
    cls.def("to_json_ast", &PyProfile::to_json_ast, "Profile JSON representation.",
            py::arg("eval_expressions") = true);

    m.def("parse", [](std::string src) { return PyProfile(std::move(src)); });
}
