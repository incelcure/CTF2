from django.contrib.auth.models import User
from django.contrib.auth.forms import UserCreationForm
from django import forms
from django.utils.safestring import mark_safe


class UserRegisterForm(UserCreationForm):
    class Meta:
        model = User
        fields = ['username', 'password1', 'password2']


class ProfileForm(forms.Form):
    color = forms.ChoiceField(label='Выберите цвет', choices=[])

    def __init__(self, *args, **kwargs):
        color_choices = kwargs.pop('color_choices', [])
        super(ProfileForm, self).__init__(*args, **kwargs)
        self.fields['color'].choices = [(color, color) for color in color_choices]
